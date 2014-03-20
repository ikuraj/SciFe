package insynth
package streams.light
package weight

import util.logging._
import util.Math._

import scala.collection.mutable.PriorityQueue

import scala.reflect._

protected[streams] class BinaryLazy[T, V, U]
	(val left: FiniteIntegerWeightEnum[T], val right: FiniteIntegerWeightEnum[V])
	(combine: (T, V) => U)(implicit ct: ClassTag[U])
	extends IntegerWeightEnum[U] with Finite[U] with HasLogger {
  
  override def size =
    if (left.size < 0 || right.size < 0) -1
    else left.size * right.size
  
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
  private[this] var currentElement: U = _
  private[this] var currentWeight: Int = _
  
  type Record = (Int, Int, Int)
	val ord = new Ordering[Record] {
	  def compare(a : Record, b : Record) = b._1 - a._1
	}
  private[this] val pq = PriorityQueue[Record]()(ord)
  pq.sizeHint(size)
  
  def makeRecord(leftInd: Int, rightInd: Int) =
    ( left.getWeight(leftInd) + right.getWeight(rightInd), leftInd, rightInd )
  
  pq.enqueue( makeRecord(0, 0) )
  
  private def next {
    nextInd += 1
    val (weight, li, ri) = pq.dequeue
    
    currentWeight = weight
    currentElement = combine(left(li), right(ri))
    
	  pq.enqueue( makeRecord(li + 1, ri + 1) )
	  if (li == 0) pq.enqueue( makeRecord(li, ri + 1) )
	  if (ri == 0) pq.enqueue( makeRecord(li + 1, ri) )
  }
  
}