package scife.enumeration
package reverse
package dependent

import scife.{ enumeration => e }

import scala.language.higherKinds

class InMap[I, NewIn, O/*, MemberType <: MemberDepend[I, O]*/](
  override val inner: ReverseDepend[I, O], override val f: NewIn => I
) extends e.dependent.combinators.InMap[I, NewIn, O] with ReverseDepend[NewIn, O] {

  override type DependType = ReverseDepend[I, O]

}
