package insynth
package streams
package unordered

import util.logging._

class FiniteStream[T](val coll: Seq[T])
	extends Streamable[T] with HasLogger {
  require(coll.hasDefiniteSize)
  
  override def isInfinite = false
  
  override def getStream = coll.toStream
  
  override def size = coll.size
    
}

class FiniteStreamArray[T](val coll: Array[T])
  extends Streamable[T] with HasLogger {
  
  override def isInfinite = false
  
  override def getStream = coll.toStream
  
  override def size = coll.size
    
}

object FiniteStream {
  def apply[T](stream: Seq[T]) =
//    if (stream.size == 1)
//      new Singleton[T](stream.head._1, stream.head._2)
//    else
    	new FiniteStream(stream)

  def apply[T](stream: Array[T]) =
//    if (stream.size == 1)
//      new Singleton[T](stream.head._1, stream.head._2)
//    else
      new FiniteStreamArray(stream)
  
}