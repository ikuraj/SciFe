package scife.enumeration
package dependent
package combinators

import scala.language.higherKinds

trait InMap[I, NewIn, O] extends Depend[NewIn, O] {

  type DependSort[A, B] <: Depend[A, B]
  type DependType = DependSort[I, O]
  override type EnumSort[C] = inner.EnumSort[C]

  val inner: DependType
  val f: NewIn => I

  override def getEnum(parameter: NewIn) =
    inner.getEnum( f(parameter) )

}
