package insynth.streams
package ordered

import scala.collection.mutable

import insynth.util.logging._
import insynth.streams.ordered.{ IntegerWeightStreamable => Streamable }
import insynth.streams.unordered.{ RoundRobbin => UnRoundRobbin }

class RoundRobbin[T] protected[streams] (val streams: Seq[IntegerWeightStreamable[T]])
	extends
//	InnerRoundRobbin[T]( streams.map(_.getValuedStream.iterator) ) with
	Streamable[T] with HasLogger {
  
  def getValuedStream =
    new InnerRoundRobbin( streams ).getValuedStream
    
  override def size =
    if (streams.exists(_.size == -1)) -1
    else streams.map(_.size).sum
}

private[streams] class InnerRoundRobbin[T] protected[streams] (streamsIn: Seq[IntegerWeightStreamable[T]])
  extends HasLogger {
  
  protected var iterators = Array(streamsIn.map(_.getValuedStream.iterator.buffered): _*)
    
  // NOTE this method must keep fairness    
  def getNextIndex(currentIndex: Int): Int = {
   
    var min = Int.MaxValue
    var minInd = -1
    
    for (ind <- 1 to iterators.size) {
      val indToCheck = (currentIndex + ind) % iterators.size
      
      fine("indToCheck " + indToCheck)
      if (iterators(indToCheck).head._2 < min) {
        fine("index passed: " + indToCheck + ", value: " + iterators(indToCheck).head)
        min = iterators(indToCheck).head._2
        minInd = indToCheck
      }
    }
        
    minInd
  }
  
  def filterIterators = 
    // check if there is at least one iterator with next element
    iterators = iterators filter { _.hasNext }
  
  def getNextAndFilter(nextIndex: Int) = {
    val res = iterators(nextIndex).next
    filterIterators
    res
  }
  
  def getValuedStream = { 
    // inner function which "produces" new elements
    // TODO wow a bug found due to the name!?
    def loop(currentIndex: Int): Stream[(T, Int)] = {
      entering("inner loop: " + currentIndex)
      
      // OPTIMIZATION: switch to the single stream if all other are out of elements
      if (iterators.size == 1)
        iterators.head.toStream
      else {
        val nextIndex = getNextIndex(currentIndex)
        
        if (nextIndex > -1)
          // prepend the element to a recursively computed stream
          // NOTE: calling next here will not evaluate to next until head is called
          getNextAndFilter(nextIndex) #:: loop(nextIndex)
        else
          // no iterator has next, return empty stream
          Stream.empty
      }
    }
        
    // start from first iterator
    loop(0)
  }
  
}

object RoundRobbin {
  def apply[T](streams: Seq[IntegerWeightStreamable[T]]) = {
//    require(streams.forall(!_.isInfinite))
  	
    // OPTIMIZATION: if streams have more than 3 elements, use PriorityQueue
    assert (streams.size < 10)
    new RoundRobbin(streams)
  }

  def memoized[T](streams: Seq[IntegerWeightStreamable[T]]) = {
    new RoundRobbin(streams) with Memoized[T]
  }
}