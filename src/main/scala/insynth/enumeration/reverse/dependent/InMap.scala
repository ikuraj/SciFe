package insynth.enumeration.reverse
package dependent

import insynth.{ enumeration => e }
import e.dependent.combinators

import scala.language.higherKinds

class InMap[I, NewIn, O/*, ReverseType <: ReverseDepend[I, O]*/](
  override val inner: ReverseDepend[I, O], override val f: NewIn => I  
) extends combinators.InMap[I, NewIn, O] with ReverseDepend[NewIn, O] {
  
  //override type DependType = ReverseType
  override type DependType = ReverseDepend[I, O]
  
}

class InMapFin[I, NewIn, O](
  override val inner: ReverseDependFinite[I, O], override val f: NewIn => I  
) extends combinators.InMap[I, NewIn, O] with ReverseDependFinite[NewIn, O] {
  
  override type DependType = ReverseDependFinite[I, O]
  
}

//object InMap {
//  
//  def apply[T, U, O](tde: DependReverse[T, O], modify: U => T) =
//	  new e.dependent.InMap(tde, modify) with DependReverse[U, O]
//  
//}