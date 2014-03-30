package insynth.enumeration
package dependent

trait Depend[I, +O] {
  
  type EnumType <: Enum[O]

  def apply(parameter: I) = getEnum(parameter)
  
  def getEnum(parameter: I): EnumType
  
}