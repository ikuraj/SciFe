package insynth.enumeration
package lzy

import scala.collection.mutable

import _root_.insynth.util.logging._

object ConcatFinite {

  def apply[T](left: Finite[T], right: Finite[T]) =
    new ConcatFiniteVariedSize(streams)

  def fixed[T](streams: Array[Finite[T]]) =
    new ConcatFiniteVariedSize(streams)

  def equal[T](streams: Seq[Finite[T]]) =
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
  
  override def size: Int

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
      if (limit(mid) <= target && limit(mid+1) > target)
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

class ConcatFiniteVariedSize[@specialized T] protected[enumeration] (val streams: Array[Finite[T]])
  extends ConcatFinite[T] with HasLogger {

  override def length = streams.length

  val limits = {
    var _size = 0
    val ab = mutable.ArrayBuffer(0)
    val tail =
      for (stream <- streams) yield {
        _size += stream.size
        _size
      }
    (ab ++ tail).toArray
  }

  override def size = {
    fine("limits = " + limits.mkString(","))
    limits.apply(length)
  }
  
  override def enum(ind: Int) = streams(ind)
  
  override def limit(ind: Int) = limits(ind)

}

// Union of finite enumerators of equal length
class ConcatFiniteEqualSize[T] protected[enumeration] (streams: Seq[Enum[T]])
  extends Finite[T] with HasLogger {
  assert(streams.map(_.size).distinct.size == 1, "RoundRobbinFiniteEqual should be constructed with streams of equal sizes." +
    "(sizes are %s)".format(streams.map(_.size).distinct))
  
  val streamsArray = streams.toArray
  
  override def apply(ind: Int) = {
    val arrInd = ind % streamsArray.size
    val elInd = ind / streamsArray.size
    streamsArray(arrInd)(elInd)
  }
    
  override def size =
    if (streams.exists(_.size == -1)) -1
    else streams.map(_.size).sum

}