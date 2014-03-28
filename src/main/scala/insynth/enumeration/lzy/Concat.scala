package insynth.enumeration
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
        ConcatInfinite( Seq(left, right) )
    }
  
  def apply[T, U <: Enum[T]](streams: Seq[U])(implicit ct: ClassTag[U]) = {
    val finiteTag = implicitly[ClassTag[Finite[_]]]
    val infiniteTag = implicitly[ClassTag[Infinite[_]]]
    implicitly[ClassTag[U]] match {
      case _: Finite[_] =>
        val seq = streams.asInstanceOf[Seq[Finite[T]]]
        ConcatFinite(seq.toArray)
      case `infiniteTag` =>
        val seq = streams.asInstanceOf[Seq[Infinite[T]]]
        ConcatInfinite(seq)
    }
  }
  
  def apply[T](finites: Array[Finite[T]]) = {
    ConcatFinite( finites )
  }
  
  def apply[T](infinites: Seq[Infinite[T]]) = {
    ConcatInfinite( infinites )
  }
  
}