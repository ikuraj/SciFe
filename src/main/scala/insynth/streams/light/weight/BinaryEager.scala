package insynth
package streams.light
package weight

import util.logging._
import util.Math._

import scala.collection.mutable.PriorityQueue

import scala.reflect._

protected[streams] class BinaryEager[T, V, U]
	(val left: FiniteIntegerWeightEnum[T], val right: FiniteIntegerWeightEnum[V])
	(combine: (T, V) => U)(implicit ct: ClassTag[U])
	extends IntegerWeightEnum[U] with Finite[U] with HasLogger {
  
  override def size =
    if (left.size < 0 || right.size < 0) -1
    else left.size * right.size
  
  override def apply(ind: Int) = elements(ind)
  
  override def getWeight(ind: Int) = weights(ind)
  
  private var elements = Array.ofDim[U](size)
  private var weights = Array.ofDim[Int](size)

	val ord = new Ordering[(U, Int)] {
	  def compare(a : (U, Int), b : (U, Int)) = b._2 - a._2
	}
  private val pq = PriorityQueue[(U, Int)]()(ord)
  pq.sizeHint(size)
  
  // according to the cantor ordering put all pairs into the PQ
  for(ind <- 0 until size) {
    val (i1, i2) = cantorInverse(ind)

    val leftElem = left(i1)
    val rightElem = right(i2)
    val leftWeight = left.getWeight(i1)
    val rightWeight = right.getWeight(i2)
    
    pq.enqueue( (combine(leftElem, rightElem), leftWeight + rightWeight) )
  }
  
  for(ind <- 0 until size; (el, weight) = pq.dequeue) {
    elements(ind) = el
    weights(ind) = weight
  }
  
}