package insynth.enumeration
package member

import insynth.{ enumeration => en }

abstract class Map[T, U](enum: Member[T], modify: T=>U, revFun: U=>T)
	extends en.Map[T, U](enum, modify) with Member[U] with HasLogger {

  override def member(a: U) = {
    info("reverse in Map is: " + revFun( a ))
    enum.member( revFun( a ) )
  }
  
}