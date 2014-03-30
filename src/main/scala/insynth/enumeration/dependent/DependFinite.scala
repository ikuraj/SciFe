package insynth.enumeration
package dependent

trait DependFinite[I, +O] extends Depend[I, O] {
  
  override type EnumType <: Finite[O]
  
}