package insynth.enumeration

import memoization._

abstract class Map[T, U](override val enum: Enum[T], override val f: T=>U)
	extends Enum[U] with combinators.Map[T, U] {

  override def size = enum.size
  
}

object Map {
  
  def apply[T, U](streamable: Enum[T], modify: T=>U) =
    streamable match {
    	case f: Finite[_] => new Map(streamable, modify) with Finite[U]
    	case i: Infinite[_] => new Map(streamable, modify) with Infinite[U]
	  }
  
  def memoized[T, U](streamable: Enum[T], modify: T=>U) = 
    streamable match {
    	case f: Finite[_] => new Map(streamable, modify) with Finite[U] with Memoized[U]
    	case i: Infinite[_] => new Map(streamable, modify) with Infinite[U] with Memoized[U]
	  }
  
  def apply[T, U](streamable: Finite[T], modify: T=>U) =
    new Map(streamable, modify) with Finite[U]
  
  def memoized[T, U](streamable: Finite[T], modify: T=>U) =
    new Map(streamable, modify) with Finite[U] with Memoized[U]
  
  def apply[T, U](streamable: Infinite[T], modify: T=>U) =
    new Map(streamable, modify) with Infinite[U]
  
}