package insynth.streams.ordered

import insynth.streams._
import insynth.streams.unordered.{ SingleStream => UnSingleStream }

// NOTE this would require ordered stream
class SingleStream[T](stream: => Stream[(T, Int)])
	extends OrderedStreamable[T] {
  
  override def isInfinite = true
  
  override def isDepleted: Boolean = false // wtv
  override def nextReady(ind: Int): Boolean = true
  
  override def getStream = stream map { _._1 }
  
  override def getValues = stream map { _._2 }
    
}

object SingleStream {
  def apply[T](stream: => Stream[(T, Int)]) =
    new SingleStream(stream)
}