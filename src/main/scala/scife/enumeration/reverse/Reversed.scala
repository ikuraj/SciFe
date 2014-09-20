package scife.enumeration
package reverse

trait Reversed[T] extends Finite[T] {

  val pos: Int

  override abstract def size = super.size - pos

  override abstract def apply(ind: Int) = {
    super.apply(ind + pos)
  }

}
