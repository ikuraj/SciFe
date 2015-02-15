package scife.enumeration
package lzy

import combinators._

import scala.collection.mutable
import scala.reflect.ClassTag

import scife.util._

object ConcatFinite {

  def apply[T](left: Finite[T], right: Finite[T])/*(implicit ct: ClassTag[T])*/ =
    if (left.size == right.size)
      new ConcatFiniteEqualSize( Array(left, right) )
    else
      new ConcatFiniteVariedSize( Array(left, right) )

  def apply[T](finites: Array[Finite[T]])/*(implicit ct: ClassTag[T])*/ =
    if (finites.map(_.size).distinct.size == 1)
      new ConcatFiniteEqualSize( finites )
    else
      new ConcatFiniteVariedSize( finites )

  def fixed[T](streams: Array[Finite[T]]) =
    new ConcatFiniteVariedSize(streams)

  def equal[T](streams: Array[Finite[T]]) =
    new ConcatFiniteEqualSize(streams)

  def buffer[T](streams: Seq[Finite[T]]) =
    new dynamic.ConcatFiniteBuffer(streams)

}

//concatenation of finite enumerators
abstract class ConcatFinite[T] protected[enumeration]
  extends Finite[T] with HasLogger {
  
  def length: Int
  
  def enum(ind: Int): Enum[T]
  
  def limit(ind: Int): Int
  
  override def apply(ind: Int) = {
   entering("apply", ind)
   val arrInd = binarySearch(ind)
   val elInd = ind - limit(arrInd)
   exiting("apply", enum(arrInd)(elInd))
  }
  
  protected[enumeration] def binarySearch(target: Int): Int = {
   var left = 0
   // limits are indexed 0..length
   var right = length
   while (left <= right) {
     val mid = (left + right) / 2
     info("target=%d, left=%d, mid=%d, right=%d".format(target, left, mid, right))
     if (limit(mid) <= target && limit(mid + 1) > target)
       return mid
     else if (limit(mid) > target)
       right = mid
     else
       left = mid
   }
   // should not happen
   throw new RuntimeException
  }

}

// deliberately not extending ConcatFinite
class ConcatFiniteVariedSize[@specialized T] protected[enumeration] (val enumArray: Array[Finite[T]])
  extends Finite[T] with ConcatMul[T] with HasLogger {

  override def enums = enumArray.toSeq

  override def apply(ind: Int) = {
   entering("apply", ind)
   val arrInd = binarySearch(ind)
   val elInd = ind - limits(arrInd)
   exiting("apply", enumArray(arrInd)(elInd))
  }

  private[this] val limits = {
    var _size = 0
    val ab = mutable.ArrayBuffer(0)
    for (stream <- enumArray) {
      _size += stream.size
      ab += _size
    }
    ab.toArray
  }

  override def size = {
    fine("limits = " + limits.mkString(","))
    limits.apply(enumArray.length)
  }

  private[enumeration] def binarySearch(target: Int): Int = {
   var left = 0
   // limits are indexed 0..length
   var right = enumArray.length
   while (left <= right) {
     val mid = (left + right) / 2
     info("target=%d, left=%d, mid=%d, right=%d".format(target, left, mid, right))
     if (limits(mid) <= target && limits(mid + 1) > target)
       return mid
     else if (limits(mid) > target)
       right = mid
     else
       left = mid
   }
   // should not happen
   throw new RuntimeException
  }
  
}

// Union of finite enumerators of equal length
class ConcatFiniteEqualSize[T] protected[enumeration] (val enumArray: Array[Finite[T]])
  extends Finite[T] with ConcatMul[T] with HasLogger {
  assert(enumArray.map(_.size).distinct.size == 1, "RoundRobbinFiniteEqual should be constructed with streams of equal sizes." +
    "(sizes are %s)".format(enumArray.map(_.size).distinct))

  override def enums = enumArray.toSeq
    
  override def size = {
    assert( enumArray.length * enumArray(0).size == super.size )
    enumArray.length * enumArray(0).size
  }

  override def apply(ind: Int) = {
    val arrInd = ind % enumArray.length
    val elInd = ind / enumArray.length
    enumArray(arrInd)(elInd)
  }

}
