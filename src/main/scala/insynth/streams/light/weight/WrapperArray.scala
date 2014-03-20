package insynth.streams
package light
package weight

import scala.reflect._
import insynth.util.logging.HasLogger

class WrapperArray[@specialized T](elements: Array[T], weights: Array[Int])
	extends light.WrapperArray[T](elements) with IntegerWeightEnum[T] with HasLogger {
  // two finite arrays of the same size
  require( elements.size == weights.size &&
    weights.hasDefiniteSize )
  // weights must be sorted
  require(weights.sorted == weights)
  
  def getWeight(ind: Int) = weights(ind)
    
}