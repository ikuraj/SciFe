package insynth.enumeration
package reverse

import insynth.{ enumeration => en }

class Map[T, U](enum: Reverse[T], modify: T=>U, revFun: U => T)
	extends en.Map[T, U](enum, modify) with Reverse[U] with HasLogger {

  override def reverse[V >: U](a: V) = {
    info("reverse in Map is: " + revFun( a.asInstanceOf[U] ))
    enum.reverse( revFun( a.asInstanceOf[U] ) )
  }
  
}