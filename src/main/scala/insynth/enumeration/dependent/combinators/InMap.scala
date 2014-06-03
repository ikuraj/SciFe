package insynth.enumeration
package dependent
package combinators

import scala.language.higherKinds

trait InMap[I, NewIn, O] extends Depend[NewIn, O] {
  
  type DependType <: Depend[I, O]
  override type EnumType = inner.EnumType
  
  val inner: DependType
  val f: NewIn => I
  
  override def getEnum(parameter: NewIn) =
    inner.getEnum( f(parameter) )

}