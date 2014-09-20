package scife.enumeration
package reverse

import scife.{ enumeration => en }

abstract class Map[T, U](enum: Reverse[T], modify: T=>U, revFun: U=>T)
	extends en.Map[T, U](enum, modify) with Reverse[U] with HasLogger {

  override def reverse(a: U) = {
    info("reverse in Map is: " + revFun( a ))
    enum.reverse( revFun( a ) )
  }
  
}

object Map {
  
  def apply[T, U](enum: Reverse[T], modify: T=>U, revFun: U=>T) =
    enum match {
      case f: Finite[_] => new Map(enum, modify, revFun) with ReverseFinite[U]
      case i: Infinite[_] => new Map(enum, modify, revFun) with ReverseInfinite[U]
    }
  
//  def memoized[T, U](streamable: Enum[T], modify: T=>U) = 
//    streamable match {
//      case f: Finite[_] => new Map(streamable, modify) with ReverseFinite[U] with Memoized[U]
//      case i: Infinite[_] => new Map(streamable, modify) with ReverseInfinite[U] with Memoized[U]
//    }
  
  def apply[T, U](enum: ReverseFinite[T], modify: T=>U, revFun: U=>T) =
    new Map(enum, modify, revFun) with ReverseFinite[U]
  
//  def memoized[T, U](streamable: Finite[T], modify: T=>U) =
//    new Map(streamable, modify) with ReverseFinite[U] with Memoized[U]
  
  def apply[T, U](enum: ReverseInfinite[T], modify: T=>U, revFun: U=>T) =
    new Map(enum, modify, revFun) with ReverseInfinite[U]
  
}