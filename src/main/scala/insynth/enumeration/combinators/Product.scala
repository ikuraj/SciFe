package insynth.enumeration
package combinators

abstract class Product[T, U](left: Enum[T], right: Enum[U]) extends Enum[(T, U)] {
  
  override def size = left.size * right.size

}