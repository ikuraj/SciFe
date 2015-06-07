package scife.enumeration
package lazytraversal

object Empty extends scife.enumeration.Empty with Skippable[Nothing] with Touchable[Nothing] with Resetable[Nothing] {
  
  override def next(i: Int) =
    throw new RuntimeException
    
}