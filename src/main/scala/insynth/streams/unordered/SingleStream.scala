package insynth.streams.unordered

import insynth.streams.Streamable

/**
 * single stream
 * @param <T>, <U>
 * @param element
 */
class SingleStream[T, U <: T](stream: => Stream[U], isInfiniteFlag: Boolean) extends Streamable[T] {
  def isInfinite = isInfiniteFlag
  override def getStream = stream
}

object SingleStream {
	def apply[T, U <: T](stream: => Stream[U], isInfiniteFlag: Boolean) =
	  new SingleStream(stream, isInfiniteFlag)
}