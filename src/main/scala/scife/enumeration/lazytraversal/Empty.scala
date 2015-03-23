package scife.enumeration
package lazytraversal

object Empty extends scife.enumeration.Empty with Skippable[Nothing] with Touchable[Nothing] {
  
  override def next(i: Int) =
    throw new RuntimeException
    
}