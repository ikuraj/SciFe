package scife.enumeration
package dependent
package combinators

import scife.enumeration.Enum

trait Chain[T, U] extends Enum[(T, U)] {

  val left: Enum[T]
  val right: Depend[T, U]

}

// NOTE: can be used as an optimization when the left element of the pair is not needed
trait ChainSingle[T, U] extends Enum[U] {

  val left: Enum[T]
  val right: Depend[T, U]

}
