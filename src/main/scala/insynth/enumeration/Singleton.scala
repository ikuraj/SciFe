package insynth.enumeration

case class Singleton[T](protected val el: T) extends Finite[T] {
  
  override def size = 1
  
  override def apply(ind: Int) = 
    if (ind == 0) el
    else throw new NoSuchElementException("Singleton has only one element, can be queried only for its first element.")
  
}