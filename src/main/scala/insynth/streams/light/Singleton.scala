package insynth.streams.light

class Singleton[T](el: T) extends Finite[T] {
  
  override def size = 1
  
  override def apply(ind: Int) = 
    if (ind == 0) el
    else throw new NoSuchElementException("Singleton has only one element")
  
}

object Singleton {

  def apply[T](element: T) = new Singleton(element)

}