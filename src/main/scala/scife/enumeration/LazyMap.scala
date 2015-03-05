package scife.enumeration

import memoization._

abstract class LazyMap[T, U](override val enum: Enum[T], override val f: (=>T)=>U)
  extends Enum[U] with combinators.LazyMap[T, U] {

}

object LazyMap {

  def apply[T, U](streamable: Finite[T], modify: (=>T)=>U) =
    new LazyMap(streamable, modify) with Finite[U]
  
}