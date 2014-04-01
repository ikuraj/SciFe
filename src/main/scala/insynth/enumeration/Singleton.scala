package insynth.enumeration

trait SingletonT[T] extends Finite[T] {
  
  val el: T
  
  override def size = 1
  
  override def apply(ind: Int): T = 
    if (ind == 0) el
    else throw new NoSuchElementException("Singleton has only one element, can be queried only for its first element.")
  
}

case class Singleton[T](override val el: T) extends SingletonT[T] {
  
}