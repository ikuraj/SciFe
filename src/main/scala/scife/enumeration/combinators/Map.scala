package scife.enumeration
package combinators

trait Map[T, U] extends Enum[U] {

  val enum: Enum[T]
  val f: T => U

  override def apply(ind: Int) =
    f( enum.apply(ind) )

}

trait LazyMap[T, U] extends Enum[U] {

  val enum: Enum[T]
  val f: (=> T) => U

  override def apply(ind: Int) =
    f( enum.apply(ind) )

  override def size = enum.size

}

