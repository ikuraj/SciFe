package insynth.streams
package light
package weight

import insynth.streams.light.Finite
import insynth.util.logging._

import scala.reflect._
import scala.annotation.tailrec

class RoundRobbinEager2[T] protected[streams]
  (left: FiniteIntegerWeightEnum[T], right: FiniteIntegerWeightEnum[T])
  (implicit ct: ClassTag[T])
  extends IntegerWeightEnum[T] with Finite[T] with HasLogger {
  require(left.size > 0 && right.size > 0)
  
  override def size =
    if (left.size < 0 || right.size < 0) -1
    else left.size + right.size
  
  override def apply(ind: Int) = elements(ind)
  
  override def getWeight(ind: Int) = weights(ind)
  
  private var elements = Array.ofDim[T](size)
  private var weights = Array.ofDim[Int](size)
  
  // assigns
  private[this] def la(leftInd: Int, ind: Int) {
  	elements(ind) = left(leftInd)
  	weights(ind) = left.getWeight(leftInd)
  }
  private[this] def ra(rightInd: Int, ind: Int) {
  	elements(ind) = right(rightInd)
  	weights(ind) = right.getWeight(rightInd)
  }
  
  @tailrec
  private[this] def merge(leftInd: Int, rightInd: Int, ind: Int): Unit = {
		if (left.getWeight(leftInd) <= right.getWeight(rightInd)) {
		  la(leftInd, ind)
		  if (leftInd + 1 == left.size)
		    for(inc <- 0 until (right.size - rightInd)) ra(rightInd + inc, ind + inc)
	    else merge(leftInd + 1, rightInd, ind + 1)
		}
		else {
		  ra(rightInd, ind)
		  if (rightInd + 1 == right.size)
		    for(inc <- 0 until (left.size - leftInd)) la(leftInd + inc, ind + inc)
	    else merge(leftInd, rightInd + 1, ind + 1)
		}
  }
  
  // hopefully this goes away after compilation
//  private[this] var leftInd = 0
//  private[this] var rightInd = 0
//  private[this] var leftElem = left(0)
//  private[this] var rightElem = right(0)
//  for (ind <- 0 until size) {
//		if (left.getWeight(leftInd) < right.getWeight(rightInd)) {
//		  elements(ind) = left(leftInd)
//		  leftInd += 1
//		  if (leftInd == size)
//		    for (ri <- rightInd until right.size) elements
//		}
//		else {
//		  elements(ind) = right(rightInd)
//		  rightInd += 1
//		}
//  }
    
}