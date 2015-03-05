package scife.enumeration.member
package dependent

import scife.{ enumeration => e }
import e.dependent.combinators

import scala.language.higherKinds

class InMap[I, NewIn, O/*, MemberType <: MemberDepend[I, O]*/](
  override val inner: MemberDepend[I, O], override val f: NewIn => I  
) extends combinators.InMap[I, NewIn, O] with MemberDepend[NewIn, O] {
  
  override type DependSort[A, B] = MemberDepend[A, B]
  
}

class InMapFin[I, NewIn, O](
  override val inner: MemberDependFinite[I, O], override val f: NewIn => I  
) extends combinators.InMap[I, NewIn, O] with MemberDependFinite[NewIn, O] {
  
  override type DependSort[A, B] = MemberDependFinite[A, B]
  
}

//object InMap {
//  
//  def apply[T, U, O](tde: DependMember[T, O], modify: U => T) =
//	  new e.dependent.InMap(tde, modify) with DependMember[U, O]
//  
//}