package scife.enumeration
package combinators

trait Concat[T, U, V] extends Enum[V] {

  val left: Enum[T]
  val right: Enum[U]

  override def size = left.size + right.size

}

trait ConcatMul[T] extends Enum[T] {
    
  def enums: Traversable[Enum[T]]

  override def size = enums.map(_.size).sum

}
