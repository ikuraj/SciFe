package insynth.enumeration
package reverse

object Empty extends Empty with Reverse[Nothing, Any] {
  
  override def reverse[Nothing](a: Nothing, par: Any) = 
    throw new UnsupportedOperationException("Cannot call reverse on an empty enumerator.")
  
}