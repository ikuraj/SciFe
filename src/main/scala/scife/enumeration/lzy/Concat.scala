package scife.enumeration
package lzy

import scala.reflect._

object Concat {

  def apply[T](left: Enum[T], right: Enum[T]) =
    (left, right) match {
      case (left: Finite[T], right: Infinite[T]) =>
        ConcatFiniteInfinite(left, right)
      case (left: Finite[T], right: Finite[T]) =>
        ConcatFinite(left, right)
      case (left: Infinite[T], right: Infinite[T]) =>
        ConcatInfinite( Array(left, right) )
    }

  def apply[T](left: Finite[T], right: Finite[T]) =
    ConcatFinite(left, right)

  def apply[T, U <: Enum[T]](streams: Array[U])(implicit ct: ClassTag[U]) = {
    val finiteTag = implicitly[ClassTag[Finite[_]]]
    val infiniteTag = implicitly[ClassTag[Infinite[_]]]
    implicitly[ClassTag[U]] match {
      case _: Finite[_] =>
        val seq = streams.asInstanceOf[Array[Finite[T]]]
        ConcatFinite(seq.toArray)
      case `infiniteTag` =>
        val seq = streams.asInstanceOf[Array[Infinite[T]]]
        ConcatInfinite(seq)
    }
  }

  def apply[T](finites: Array[Finite[T]]) = {
    ConcatFinite( finites )
  }

  def apply[T](infinites: Array[Infinite[T]]) = {
    ConcatInfinite( infinites )
  }

}
