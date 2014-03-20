package insynth.streams
package light
package weight

import scala.reflect._
import scala.language.implicitConversions

trait WeightEnum[@specialized +A, @specialized(Int, Float) V] extends Enum[A] with Weighted[V] {

}

trait WeightEnumPair[@specialized +A, @specialized(Int, Float) V] extends Enum[(A, V)] with Weighted[V] {
    
  def getWeight(ind: Int) = this(ind)._2
  
}