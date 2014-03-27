package insynth.enumeration

object Identity extends Infinite[Int] {
  
  override def apply(ind: Int) = ind
  
}