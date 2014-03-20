package insynth.streams
package light
package weight

import insynth.streams._
import insynth.streams.unordered.{ SingleStream => UnSingleStream }

/**
 * Wrapper around the Scala stream
 * NOTE: parameter stream needs to be ordered itself
 */
class WrapperStream[T](elements: Stream[T], weights: Stream[Int])
	extends light.WrapperStream[T](elements) with IntegerWeightEnum[T] with Infinite[T] {
  
  def getWeight(ind: Int) = weights(ind)

}