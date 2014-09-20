package scife.enumeration
package lzy

import combinators._

import scala.collection.mutable
import scala.reflect.ClassTag

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

// concatenation of finite enumerators
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

  // writing our own binary search since we need 2.10
  // (binary search available from 2.11)
  def binarySearch(target: Int): Int = {
    var left = 0
    // limits are indexed 0..length
    var right = length
    while (left <= right) {
      val mid = left + (right - left) / 2
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

class ConcatFiniteVariedSize[@specialized T] protected[enumeration] (enumsArray: Array[Finite[T]])
  extends ConcatFinite[T] with ConcatMul[T, T, T] with HasLogger {

  override val enums = enumsArray.toSeq

  override def length = enumsArray.length

  val limits = {
    var _size = 0
    val ab = mutable.ArrayBuffer(0)
    val tail =
      for (stream <- enumsArray) yield {
        _size += stream.size
        _size
      }
    (ab ++ tail).toArray
  }

  override def size = {
    fine("limits = " + limits.mkString(","))
    limits.apply(length)
  }

  override def enum(ind: Int) = enumsArray(ind)

  override def limit(ind: Int) = limits(ind)

}

// Union of finite enumerators of equal length
class ConcatFiniteEqualSize[T] protected[enumeration] (enumsArray: Array[Finite[T]])
  extends Finite[T] with ConcatMul[T, T, T] with HasLogger {
  assert(enumsArray.map(_.size).distinct.size == 1, "RoundRobbinFiniteEqual should be constructed with streams of equal sizes." +
    "(sizes are %s)".format(enumsArray.map(_.size).distinct))

  override val enums = enumsArray.toSeq

  val streamsArray = enumsArray
  val streamsArraySize = enumsArray.size

  override def apply(ind: Int) = {
    val arrInd = ind % streamsArraySize
    val elInd = ind / streamsArraySize
    streamsArray(arrInd)(elInd)
  }

}
