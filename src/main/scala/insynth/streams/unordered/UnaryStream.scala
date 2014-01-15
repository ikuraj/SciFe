package insynth.streams.unordered

import insynth.streams.Streamable

/**
 * unary modifying stream, applies given function to each element of the given stream to produce
 * a new stream
 * @param <T>, <U>
 * @param element
 */
class UnaryStream[T, U](val streamable: Streamable[T], modify: T=>U) extends Streamable[U] {
  
  override def size = streamable.size
  
  override def isInfinite = streamable.isInfinite
  
  lazy val memoizedStream = {
//    // inner function which "produces" new elements
//    def loop(currentStream: Stream[T]): Stream[U] =
//      if (currentStream.isEmpty)
//      	Stream.empty
//      else
//        modify(currentStream.head) #:: loop(currentStream.tail)
//			  
//	  // start with first iterator
//    loop(streamable.getStream)
    
    streamable.getStream map { modify(_) }
  }
  
  override def getStream = memoizedStream
}

object UnaryStream {
	def apply[T, U](streamable: Streamable[T], modify: T=>U) = 
	  new UnaryStream(streamable, modify)
}