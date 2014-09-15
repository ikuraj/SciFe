package scife.enumeration
package combinators

trait Product[T, U] extends Enum[(T, U)] {
  
  val left: Enum[T]
  val right: Enum[U]
  
  override def size = left.size * right.size

}