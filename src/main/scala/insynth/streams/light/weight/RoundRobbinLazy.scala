package insynth.streams
package light
package weight

import insynth.streams.light.Finite
import insynth.util.logging._

import scala.reflect._
import scala.annotation.tailrec

class RoundRobbinLazy[T] protected[streams]
  (left: FiniteIntegerWeightEnum[T], right: FiniteIntegerWeightEnum[T])
  (implicit ct: ClassTag[T])
  extends IntegerWeightEnum[T] with Finite[T] with HasLogger {
  require(left.size > 0 && right.size > 0)
  
  override def size =
    if (left.size < 0 || right.size < 0) -1
    else left.size + right.size
  
  override def apply(ind: Int) = {
    require (ind == nextInd)
    next
    currentElement
  }
  
  override def getWeight(ind: Int) = {    
    require (ind + 1 == nextInd)
    currentWeight
  }
  
  private[this] var nextInd: Int = 0
  private[this] var currentElement: T = _
  private[this] var currentWeight: Int = _
  
  private def la {
  	currentElement = left(leftInd)
  	currentWeight = left.getWeight(leftInd)
  	leftInd += 1
  }
  private def ra {
  	currentElement = right(rightInd)
  	currentWeight = right.getWeight(rightInd)
  	rightInd += 1
  }
  
  private def next {
    nextInd += 1
    
    if (leftInd >= left.size) ra
    else if (rightInd >= right.size) la
    else if (left.getWeight(leftInd) <= right.getWeight(rightInd)) la
		else ra
  }
  
  private[this] var leftInd = 0
  private[this] var rightInd = 0
    
}