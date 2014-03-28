package insynth.enumeration

abstract class Map[T, U](val enum: Enum[T], modify: T=>U)
	extends combinators.Map(enum, modify) {

  override def size = enum.size
  
}

object Map {
  
  def apply[T, U](streamable: Enum[T], modify: T=>U) =
    streamable match {
    	case f: Finite[_] => new Map(streamable, modify) with Finite[U]
    	case i: Infinite[_] => new Map(streamable, modify) with Infinite[U]
	  }
  
}