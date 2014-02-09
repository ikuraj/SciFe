package insynth.streams
package dependent

import scala.collection.mutable.{ ArrayBuffer, MutableList }

import insynth.util.logging._

import scala.language.postfixOps
import scala.annotation._

case class Binary[I, O1, I2, O2, O]
  (s1: DependentStreamable[I, O1], s2: DependentStreamable[I2, O2])
  (chain: O1 => I2) (combine: (O1, O2) => O) extends DependentStreamable[I, O] with HasLogger {
  
  override def getStream(parameter: I) = {      
    entering("getStream", parameter)
    val leftStream = s1.getStream(parameter)
    new InnerBinaryStream(leftStream) getNext 0
  }
	  
	private[Binary] class InnerBinaryStream(var s1: Stream[O1]) extends HasLogger {
    type ValuePairLeft = O1
    type ValuePairRight = O2
    type ValuePairOut = O
	  
	  /**
	   * Returns index of the iterator with minimal weight
	   * NOTE: fairness is guaranteed by keeping track of which iterator was forwarded previously
	   * @param lastIndex index of previously forwarded iterator
	   * @return index of the iterator with minimal weight
	   */
	  final def getNext(lastIndex: Int): Stream[ValuePairOut] = {
	    entering("getNext", lastIndex)
	    
	    val nextIndex =
	      if( lastIndex + 1 >= iterators.size ) {
	        iterators append newLeftStream.iterator.buffered
	        // NOTE: no need to check we assume streams are infinite
	        s1 = s1.tail
	        0
	      } else
	        lastIndex + 1
	            
      iterators(nextIndex).next #:: getNext(nextIndex)
	  }
	  
	  /** Stream that streams over combinations of two streams with indexes ind1, ind2 such that
	   *  ind2 >= ind1 at all times and ind1 is fixed 
	   * 	It can add a new iterator with left index equal to ind1 + 1 if it has next lowest
	   *  sum of values */
	  def newLeftStream: Stream[ValuePairOut] = {
	    entering("newLeftStream", s1)
	    
	    // store first element from first stream
	    val leftElem = s1.head
	    val rightStream = s2.getStream( chain(leftElem) )
	      
	    rightStream map { p => combine(leftElem, p) }
	  }
	  
//	  /** Stream that streams over combinations of two streams with indexes ind1, ind2 such that
//	   *  ind2 < ind1 at all times and ind1 is fixed 
//	   * 	It can produce a new iterator with higher ind1 if it can bring lower
//	   *  sum of values (cannot be equal as in the leftStream case) */
//	  def rightStream(ind: Int, s1: Stream[ValuePairLeft],
//	    s2: Stream[ValuePairRight]): Stream[ValuePairOut] = {
//	    entering("rightStram", ind, s1, s2)
//	
//	    // store right element from right stream
//	    val rightHead = s2.head
//	    val leftHead = s1.head
//	
////	    val producePartial: ValuePairLeft => ValuePairOut =
////	      combineTwoValues(_, rightPair)
//	
//	    combine(leftHead, rightHead) #:: {
//	      if (!s1.tail.isEmpty && !s2.tail.isEmpty)
//	      	iteratorsToBeAdded += rightStream(ind + 1, s1.tail, s2.tail).iterator.buffered
//	    	s1.tail map  { p => combine(p, rightHead) }
//	    }
//	  }
	
	  val INITIAL_ARRAYBUF_SIZE = 16
	  // set of iterators checked for next value
	  var iterators = new ArrayBuffer[BufferedIterator[ValuePairOut]](INITIAL_ARRAYBUF_SIZE)
	  // shadow copy for fast swapping
	  var iterators_shadow = new ArrayBuffer[BufferedIterator[ValuePairOut]](INITIAL_ARRAYBUF_SIZE)
	  
	  private def swapIterators = {
	    val tmp = iterators
	    iterators = iterators_shadow
	    iterators_shadow = tmp
	    iterators_shadow.clear
	  }
	  
	  // iterators to be added in the next iteration
	  var iteratorsToBeAdded = MutableList[BufferedIterator[ValuePairOut]]()
	  
	  def addToIterators(it: BufferedIterator[ValuePairOut]) = {
	    iteratorsToBeAdded += it
	  }
	  
	}
  
}