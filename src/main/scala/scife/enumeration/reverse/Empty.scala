package scife.enumeration
package reverse

import _root_.scife.{ enumeration => e }

object Empty extends e.Empty with ReverseFinite[Nothing] {

  override def reverse(a: Nothing) =
    throw new NoSuchElementException("Cannot call reverse on an empty enumerator.")

}
