package scife.enumeration
package dependent

trait DependInfinite[I, +O] extends Depend[I, O] {
  
  override type EnumType <: Infinite[O]
  
}