package insynth.enumeration

trait Finite[+A] extends Enum[A] {
  
  def hasDefiniteSize = true
  
}