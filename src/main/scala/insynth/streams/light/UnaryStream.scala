package insynth.streams.light

/**
 * A Enum that maps values of a given Enum with the given function 
 * NOTE: modifyVal should be a monotonic function, otherwise your computer will blow up!
 * @param <T> @see Enum
 */
class Mapper[T, U](val streamable: Enum[T], modify: T=>U)
	extends Enum[U] {

  override def hasDefiniteSize = streamable.hasDefiniteSize
  
  override def size = streamable.size
  
  override def apply(ind: Int) =
    modify( streamable(ind) )
  
}

object Mapper {
  
  // TODO these overrides are a hack
  def apply[T, U](streamable: Enum[T], modify: T=>U) =
    streamable match {
    	case f: Finite[_] => new Mapper(streamable, modify) with Finite[U] {
    		override def hasDefiniteSize = true    	  
    	}
    	case i: Infinite[_] => new Mapper(streamable, modify) with Infinite[U] {
    		override def hasDefiniteSize = false
    	}
	  }
  
}