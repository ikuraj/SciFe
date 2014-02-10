package insynth.streams.light

/**
 * Wrapper around the Scala stream
 * NOTE: parameter stream needs to be ordered itself
 */
class WrapperStream[T](stream: Stream[T])
	extends Infinite[T] {
  
  override def apply(ind: Int) =
    stream(ind)
    
  override def size = -1
}