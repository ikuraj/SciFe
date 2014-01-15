package insynth.streams.ordered

import insynth.streams._
import insynth.streams.unordered.{ SingleStream => UnSingleStream }

class TestSingleStream[T](stream: => Stream[(T, Int)], isInfiniteFlag: Boolean)
	extends WrapperStream[T](stream) {
  
  val itVal = (stream map { _._2 }).iterator.buffered
  val itStr = (stream map { _._1 }).iterator.buffered
  
  override def isInfinite = isInfiniteFlag
  
  def isDepleted: Boolean =
    false
      
  def nextReady(ind: Int): Boolean =
    if (isInfiniteFlag) true
    else ind < stream.size
  
}

protected object TestSingleStream {
  def apply[T](stream: => Stream[(T, Int)], isInfiniteFlag: Boolean = false) =
    new TestSingleStream(stream, isInfiniteFlag)
}