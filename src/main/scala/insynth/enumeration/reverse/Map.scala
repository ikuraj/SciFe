package insynth.enumeration
package reverse

import insynth.{ enumeration => en }

class Map[T, U](enum: Reverse[T], modify: T=>U, revFun: U => T)
	extends en.Map[T, U](enum, modify) with Reverse[U] {

  override def reverse[V >: U](a: V) =
    enum.reverse( revFun( a.asInstanceOf[U] ) )
  
}