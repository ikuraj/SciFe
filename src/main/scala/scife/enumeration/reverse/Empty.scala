package scife.enumeration
package reverse

import _root_.scife.{ enumeration => e }

class Empty[T] extends e.Empty with ReverseFinite[T] {

  override def reverse(a: T) =
    throw new NoSuchElementException("Cannot call reverse on an empty enumerator.")

}
