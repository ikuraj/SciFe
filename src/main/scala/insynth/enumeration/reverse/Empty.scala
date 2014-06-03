package insynth.enumeration
package reverse

import _root_.insynth.{ enumeration => e }

class Empty[T] extends e.Empty with ReverseFinite[T] {
  
  override def reverse(a: T) = 
    throw new NoSuchElementException("Cannot call reverse on an empty enumerator.")
  
}