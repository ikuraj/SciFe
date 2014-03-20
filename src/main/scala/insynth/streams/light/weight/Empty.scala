package insynth.streams
package light
package weight

object Empty extends light.Empty with IntegerWeightEnum[Nothing] {
 
  override def getWeight(ind: Int) =
    throw new NoSuchElementException("no elements in Empty")

}