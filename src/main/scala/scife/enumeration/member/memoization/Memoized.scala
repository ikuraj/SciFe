package scife.enumeration.member
package memoization

import scife.enumeration.{ memoization => em }

import scife.util._

import scala.collection.mutable._

trait Memoized[T] extends Member[T] with HasLogger {
  
  self: em.Memoizable =>
    
//  override lazy val logger =
//    loggerFactory.newLogger("scife.enumeration.member.memoization.Memoized")

  private[enumeration] val members = HashSet[T]()

  abstract override def apply(ind: Int): T = {
    val res: T = super.apply(ind)
    members += res
    res
  }
  
  private var fullyInitialized = false

  abstract override def member(el: T): Boolean = {
//    entering("member", el)
    if (members contains el) {
//      info(s"member $el answered from memoization")
      true
    }
    else {
      if (fullyInitialized) return false
      if (members.size == this.size) {
//        info(s"member $el not present answered from memoization")
        fullyInitialized = true
        return false
      }

      info(s"[!] member $el not memoized")
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