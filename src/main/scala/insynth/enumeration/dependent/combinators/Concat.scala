package insynth.enumeration
package dependent
package combinators

trait Concat[I, T, U, V] extends Depend[I, V] {
  
  val left: Depend[I, T]
  val right: Depend[I, U]
  
}