package scife.enumeration
package reverse
package dependent

import scife.enumeration.dependent._

trait DependReverse[I, +O] extends DependFinite[I, O] {
  
  type EnumType <: Reverse[O]

  override def apply(parameter: I): EnumType = getEnum(parameter)

  override def getEnum(parameter: I): EnumType
  
}