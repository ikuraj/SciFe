package insynth.streams.ordered

import insynth.streams._
import insynth.streams.unordered.{ SingleStream => UnSingleStream }

// NOTE this would require ordered stream
protected class SingleStream[T](stream: => Stream[(T, Int)], isInfiniteFlag: Boolean)
	extends OrderedStreamable[T] {
  
  override def isInfinite = isInfiniteFlag
  
  override def isDepleted: Boolean = throw new RuntimeException // wtv
  override def nextReady(ind: Int): Boolean = throw new RuntimeException
  
  override def getStream = stream map { _._1 }
  
  override def getValues = stream map { _._2 }
    
}

protected object SingleStream {
  def apply[T](stream: => Stream[(T, Int)], isInfiniteFlag: Boolean = false) =
    new SingleStream(stream, isInfiniteFlag)
}