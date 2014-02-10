package insynth.streams
package light

import scala.collection.mutable

import insynth.util.logging._

abstract class RoundRobbinFinite[T] protected[streams] ()
  extends Finite[T] with HasLogger {

  def length: Int
  
  def enum(ind: Int): Enumerable[T]
  
  def limit(ind: Int): Int
  
  override def size: Int

  override def apply(ind: Int) = {
    entering("apply", ind)
    val arrInd = binarySearch(ind)
    val elInd = ind - limit(arrInd)
    exiting("apply", enum(arrInd)(elInd))
  }

  // writing our own binary search since we need 2.10
  protected[light] def binarySearch(target: Int): Int = {
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

class RoundRobbinFiniteFixed[@specialized T] protected[streams] (val streams: Array[Enumerable[T]])
  extends RoundRobbinFinite[T] with HasLogger {

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
  fine("limits = " + limits)

  override def size = 
    limits.apply(length-1)
  
  override def enum(ind: Int) = streams(ind)
  
  override def limit(ind: Int) = limits(ind)

}

class RoundRobbinFiniteBuffer[T] protected[streams] (streams: Seq[Enumerable[T]])
  extends RoundRobbinFinite[T] with HasLogger {

  private var streamsArray = mutable.ArrayBuffer(streams: _*)
  
  override def length = streamsArray.length

  var _size = 0
  private var limits =
    mutable.ArrayBuffer(0) ++= {
      for (stream <- streams) yield {
        _size += stream.size
        _size
      }
    }

  def append(en: Enumerable[T]) {
    streamsArray += en
    _size += en.size
    limits += _size
  }

  override def size = _size

  override def enum(ind: Int) = streamsArray(ind)
  
  override def limit(ind: Int) = limits(ind)

}

class RoundRobbinFiniteEqual[T] protected[streams] (streams: Seq[Enumerable[T]])
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

object RoundRobbinFinite {

  def fixed[T](streams: Array[Enumerable[T]]) =
    new RoundRobbinFiniteFixed(streams)

  def buffer[T](streams: Seq[Enumerable[T]]) =
    new RoundRobbinFiniteBuffer(streams)

  def equal[T](streams: Seq[Enumerable[T]]) =
    new RoundRobbinFiniteEqual(streams)

}