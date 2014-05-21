package insynth.enumeration

trait Infinite[+A] extends Enum[A] {
  
  def hasDefiniteSize = false
  
  override def size =
    throw new UnsupportedOperationException("Getting size of an infinite" +
      " enumerator will most probably result in diverging computation.")
  
}