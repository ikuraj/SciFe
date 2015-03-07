package scife.enumeration

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

  def memoized[T, U](streamable: Enum[T], modify: T=>U)(implicit ct: scala.reflect.ClassTag[U]) =
    streamable match {
      case f: Finite[_] =>
        new {
          override val classTagT = ct
        } with Map(streamable, modify) with Finite[U] with MemoizedSize with MemoizedStatic[U]
      case i: Infinite[_] => new Map(streamable, modify) with Infinite[U] with MemoizedDynamic[U] with MemoizedSize
    }

  def apply[T, U](streamable: Finite[T], modify: T=>U) =
    new Map(streamable, modify) with Finite[U]

  def memoized[T, U](streamable: Finite[T], modify: T=>U)(implicit ct: scala.reflect.ClassTag[U]) =
  {
    require(ct != null)
    new {
      override val classTagT = ct
    } with Map(streamable, modify) with Finite[U] with MemoizedSize with MemoizedStatic[U]
  }

  def apply[T, U](streamable: Infinite[T], modify: T=>U) =
    new Map(streamable, modify) with Infinite[U]

}
