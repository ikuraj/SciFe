package scife.enumeration
package iterable
package lzy

object Empty extends scife.enumeration.Empty with ResetIter[Nothing] with Touchable[Nothing] {
  
  override def next =
    throw new RuntimeException
    
  override def hasNext = false
    
}