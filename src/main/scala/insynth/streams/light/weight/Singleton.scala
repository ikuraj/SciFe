package insynth.streams
package light
package weight

class Singleton[T](el: T, weight: Int) extends light.Singleton(el)
	with WeightEnum[T, Int] with Finite[T] {
  
  def this(pair: (T, Int)) = this(pair._1 , pair._2)
  
  override def getWeight(ind: Int) =
    if (ind == 0) weight
    else throw new NoSuchElementException("Singleton has only one element")
  
}