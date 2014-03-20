package insynth.streams
package light

object MemoizedEmpty extends Enum[Nothing] with Memoizable {
    
  override def size = 0

  override def hasDefiniteSize = true

  override def apply(ind: Int) = throw new NoSuchElementException("no elements in Empty")
  
  override def clearMemoization { }

}