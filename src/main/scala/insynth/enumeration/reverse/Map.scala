package insynth.enumeration
package reverse

import insynth.{ enumeration => en }

class Map[I, T, U](enum: Reverse[I, T], modify: T=>U, revFun: U => T)
	extends en.Map[T, U](enum, modify) with Reverse[I, U] {

  override def reverse[V >: U](a: V, par: I) =
    en.Map( enum.reverse( revFun( a.asInstanceOf[U] ), par ), modify)
  
}