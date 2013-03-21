package insynth.streams.ordered

import insynth.streams._
import insynth.streams.unordered.{ UnaryStream => UnUnaryStream }

// give modifyVal only when a monotonic function, otherwise your computer will blow up!
class UnaryStream[T, U](val streamable: OrderedStreamable[T], modify: T=>U, modifyVal: Option[Int => Int] = None/*= identity */)
	extends OrderedStreamable[U] {
  
  override def isInfinite = streamable.isInfinite
  
  override def isDepleted: Boolean = streamable.isDepleted // wtv
  override def nextReady(ind: Int): Boolean = streamable.nextReady(ind)
    
  lazy val memoizedStream =
    streamable.getStream map { modify(_) }
  
  override def getStream = memoizedStream
  
  override def getValues = 
    modifyVal match {
	    case None => streamable.getValues
	    case Some(f) => streamable.getValues.map(f)
  	} 
  
}

object UnaryStream {
  def apply[T, U](streamable: OrderedStreamable[T], modify: T=>U, modifyVal: Option[Int => Int] = None) =
    new UnaryStream(streamable, modify, modifyVal)
}