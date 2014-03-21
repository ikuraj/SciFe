package insynth.streams.light

object Identity extends Infinite[Int] {
  
  override def apply(ind: Int) = ind
  
}