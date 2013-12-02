package insynth.streams.unordered

import insynth.streams.Streamable

/**
 * filtering stream, applies given function to filter out stream elements
 * @param <T>
 */
class FilterStream[T](val streamable: Streamable[T], filterFunction: T=>Boolean) extends Streamable[T] {
  override def isInfinite = streamable.isInfinite
  
  lazy val memoizedStream =     
    streamable.getStream filter filterFunction
  
  override def getStream = memoizedStream
}

object FilterStream {
	def apply[T, U](streamable: Streamable[T], filterFunction: T=>Boolean) = 
	  new FilterStream(streamable, filterFunction)
}