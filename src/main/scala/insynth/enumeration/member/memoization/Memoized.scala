package insynth.enumeration.member
package memoization

import insynth.enumeration.{ memoization => em }

import insynth.util.logging.HasLogger

import scala.collection.mutable._

trait Memoized[T] extends Member[T] {
  
  this: em.Memoized[T] =>
  
  private[enumeration] val members = HashSet[T]()
  
  override abstract def apply(ind: Int) = {
    val res = super.apply(ind)
    members += res
    res
  }
  
  override abstract def member(el: T) = {
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
    this.clearMemoization
    members.clear
  }
  
}