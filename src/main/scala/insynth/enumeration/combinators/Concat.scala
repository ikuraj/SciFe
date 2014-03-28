package insynth.enumeration
package combinators

abstract class Concat[T, U, V](left: Enum[T], right: Enum[U]) extends Enum[V] {
  
  override def size = left.size + right.size

}