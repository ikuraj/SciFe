package insynth.enumeration
package lzy

import scala.reflect._

object Concat {
  
  def apply[@specialized T](left: Enum[T], right: Enum[T]) =
    (left, right) match {
      case (left: Finite[T], right: Infinite[T]) =>        
        ConcatFiniteInfinite(left, right)
      case _ =>
        ConcatFinite(left, right)
    }
}