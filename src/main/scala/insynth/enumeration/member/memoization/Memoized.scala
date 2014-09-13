package insynth.enumeration.member
package memoization

import insynth.enumeration.{ memoization => em }

import insynth.util.logging.HasLogger

import scala.collection.mutable._

trait Memoized[T] extends Member[T] {

  self: em.Memoizable =>

  private[enumeration] val members = HashSet[T]()

  abstract override def apply(ind: Int): T = {
    val res: T = super.apply(ind)
    members += res
    res
  }

  abstract override def member(el: T): Boolean = {
    if (members contains el) true
    else {
      if (super.member(el)) {
        // NOTE: cannot memoize this according to its index because we do not know the index
        members += el
        true
      }
      else false
    }
  }

  override def clearMemoization {
    self.clearMemoization
    members.clear
  }

  // helper (debugging) method
  protected[member] def isMemoized(el: T) = members contains el

}