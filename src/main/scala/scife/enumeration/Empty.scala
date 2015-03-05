package scife.enumeration

trait Empty extends Finite[Nothing] {

  override def size = 0

  override def apply(ind: Int) = throw new NoSuchElementException("no elements in Empty")

}

object Empty extends Empty
