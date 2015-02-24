package scife.enumeration
package iterable

import scife.{ enumeration => e }
import dependent._

import scife.util.logging._

import scala.language.higherKinds

object Chain {
  
  type DependResetIter[I, O] = e.dependent.DependFinite[I, O] {
    type EnumSort[A] <: ResetIterFinite[A]
  }

  def apply[I, O, R](s1: Finite[I], s2: DependResetIter[I, O], combine: (I, O) => R):
    ResetIterFinite[R] = {
//    (s1, s2) match {
//      case (Empty, _) => Empty
//      case (s: Singleton[I], df: DependFinite[I, O]) =>
//        Map( df.apply(s.el), { (v: O) => combine(s.el, v) } )
//      case (f: Finite[I], df: DependFinite[I, O]) =>
        new e.dependent.ChainFiniteCombine(s1, s2, combine) with ResetIter[R]
//    }
  }

}
