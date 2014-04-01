package insynth.enumeration
package reverse

object Empty extends Empty with Reverse[Nothing] {
  
  override def reverse[Nothing](a: Nothing) = 
    this
//    throw new UnsupportedOperationException("Cannot call reverse on an empty enumerator.")
  
}