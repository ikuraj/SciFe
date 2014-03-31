package insynth.enumeration
package dependent
package combinators

import insynth.enumeration.Enum

trait Chain[T, U] extends Enum[U] {
  
  val left: Enum[T]
  val right: Depend[T, U]
  
}