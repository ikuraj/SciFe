package insynth.streams.light

trait Empty extends Enum[Nothing] {
    
  override def size = 0

  override def hasDefiniteSize = true

  override def apply(ind: Int) = throw new NoSuchElementException("no elements in Empty")

}

object Empty extends Empty