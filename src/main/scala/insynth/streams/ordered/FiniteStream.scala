package insynth
package streams
package ordered

import unordered.{ SingleStream => UnSingleStream }
import util.logging._

class FiniteStream[T](coll: => Seq[(T, Int)])
	extends IntegerWeightStreamable[T] with HasLogger {
  require(coll.hasDefiniteSize)
  require(coll.sortBy(_._2) == coll, "Given collection must be sorted")
  
  override def isInfinite = false
  
  override def getValuedStream = coll.toStream
  
  override def size = coll.size
    
}

object FiniteStream {
  def apply[T](stream: => Seq[(T, Int)]) =
//    if (stream.size == 1)
//      new Singleton[T](stream.head._1, stream.head._2)
//    else
    	new FiniteStream(stream)
  
  def memoized[T](stream: => Seq[(T, Int)]) =
    if (stream.size == 1)
      new Singleton[T](stream.head) with Memoized[T]
    else
      new FiniteStream(stream) with Memoized[T]
  
  def counted[T](stream: => Seq[(T, Int)]) =
    if (stream.size == 1)
      new Singleton[T](stream.head) with OrderedCountable[T]
    else
    	new FiniteStream(stream) with OrderedCountable[T]
}