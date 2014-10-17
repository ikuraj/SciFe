package scife.enumeration
package iterable

import dependent._

import scife.util.logging._

object Chain {
  
  type FiniteResetIter[A] = Finite[A] with ResetIter[A]
  
  type DependResetIter[I, O] = DependFinite[I, O] {
    
    type EnumType <: FiniteResetIter[O]
    
  }

  def apply[I, O, R](s1: Finite[I], s2: DependFinite[I, O], combine: (I, O) => R):
    FiniteResetIter[R] = {
//    (s1, s2) match {
//      case (Empty, _) => Empty
//      case (s: Singleton[I], df: DependFinite[I, O]) =>
//        Map( df.apply(s.el), { (v: O) => combine(s.el, v) } )
//      case (f: Finite[I], df: DependFinite[I, O]) =>
        new ChainFiniteCombine(s1, s2, combine) with ResetIter[R]
//    }
  }

}
