package insynth.streams
package ordered

import scala.collection.mutable
import scala.collection.mutable.{ ArrayBuffer, MutableList }

import insynth.util.logging._
import insynth.streams.ordered.{ IntegerWeightStreamable => Streamable }

import scala.language.postfixOps

class BinaryStream[T, V, U](val s1: Streamable[T], val s2: Streamable[V])
	(combine: (T, V) => U)
	extends Streamable[U] with HasLogger with CountedLogger {
  
  type ValuePairLeft = (T, Int)
  type ValuePairRight = (V, Int)
  type ValuePairOut = (U, Int)
  
  override def size =
    if (s1.size <= -1 || s2.size <= -1) -1
    else s1.size * s2.size
    
  override def getValuedStream = {      
    entering("getValuedStream")
    new InnerBinaryStream getValuedStream
  }
	  
  def combineTwoValues( v1: ValuePairLeft, v2: ValuePairRight ) =
    ( combine(v1._1, v2._1), v1._2 + v2._2 )
	  
  def combineTwoValuesRight( v1: T, w1: Int, v2: ValuePairRight ) =
    ( combine(v1, v2._1), w1 + v2._2 )
	  
  def combineTwoValuesLeft( v1: ValuePairLeft, v2: V, w2: Int ) =
    ( combine(v1._1, v2), v1._2 + w2 )

	class InnerBinaryStream extends HasLogger with CountedLogger {
      
	  def getValuedStream = {
	    // picks next element from all "active" iterators according to weight 
	    // NOTE: index to ensure fairness in choosing the next value
//	    def loop(lastIndex: Int): Stream[ValuePairOut] = {
//	      val minInd = getMinIterator(lastIndex)
//		    
//	      if (minInd >= 0) {
//		      // advance the next iterator
//			    val nextValue = iterators(minInd).next
//			    info("next value is " + nextValue)
//			    // filter iterators
//			    swapIterators
//			    
//	        nextValue #:: loop(minInd)
//	      }
//	      else
//	        Stream.empty
//	    }
	    
	    val s1stream = s1.getValuedStream
	    val s2stream = s2.getValuedStream
	    // we start with enumerating first element from leftStream, i.e. s1(0):s2(0)
	    val initialStream = leftStream(0, s1stream, s2stream)
	
	    // stream combines head elements of both streams
	    initialStream.head #:: {
	      // after enumeration of head, add the first left/right stream iterators
	      addToIterators( initialStream.tail.iterator.buffered )
	      if (!s1stream.tail.isEmpty)
	      	addToIterators( rightStream(0, s1stream.tail, s2stream).iterator.buffered )
	      getNext(0)
	    }
	  }
	  
	  /**
	   * Returns index of the iterator with minimal weight
	   * NOTE: fairness is guaranteed by keeping track of which iterator was forwarded previously
	   * @param lastIndex index of previously forwarded iterator
	   * @return index of the iterator with minimal weight
	   */
	  private def getNext(lastIndex: Int): Stream[ValuePairOut] = {
	    entering("getNext", lastIndex)
	    
	    // at the end -1 means no next element was found
	    var minValue = Int.MaxValue
	    var result = Stream[ValuePairOut]()
	            
	    // add all pending iterators
	    iterators appendAll iteratorsToBeAdded
	    iteratorsToBeAdded = MutableList.empty
	    info( "iterators.size = " + iterators.size )
	    addValue("getNext", iterators.size)
	    
	    fine("iterators = " + iterators.zipWithIndex.filter(_._1.hasNext).
	      map(p => p._2 + ":" + p._1.head).mkString(", "))
	    // check all iterators by going from first next after previously forwarded
	    for (
	      ind <- 1 to iterators.size;
	      indToCheck = (lastIndex + ind) % iterators.size;
	      iterator = iterators(indToCheck);
	      if iterator.hasNext
	    ) {
	      if ( iterator.head._2 < minValue ) {
	        minValue = iterator.head._2
	        result = iterator.head #:: {
	          iterator.next
	          getNext(indToCheck)
	        }
	      }
	      
	      iterators_shadow += iterator
	    }
	
	    exiting("getNext", result)
	  }
	  
	  /** Stream that streams over combinations of two streams with indexes ind1, ind2 such that
	   *  ind2 >= ind1 at all times and ind1 is fixed 
	   * 	It can add a new iterator with left index equal to ind1 + 1 if it has next lowest
	   *  sum of values */
	  def leftStream(ind: Int, s1: Stream[ValuePairLeft],
	    s2: Stream[ValuePairRight]): Stream[ValuePairOut] = {
	    entering("leftStream", ind, s1, s2)
	    
	    // store first element from first stream
	    val leftPair@(leftElem, leftWeight) = s1.head
	    val rightPair@(rightElem, rightWeight) = s2.head
	      
//	    val producePartial: ValuePairRight => ValuePairOut =
//	      combineTwoValues(leftPair, _)
	
	    ( combine(leftElem, rightElem), leftWeight + rightWeight ) #:: {
	      if (!s1.tail.isEmpty && !s2.tail.isEmpty)
	      	iteratorsToBeAdded += leftStream(ind + 1, s1.tail, s2.tail).iterator.buffered
	    	s2.tail map { p => combineTwoValuesRight(leftElem, leftWeight, p) }
	    }
	  }
	  
	  /** Stream that streams over combinations of two streams with indexes ind1, ind2 such that
	   *  ind2 < ind1 at all times and ind1 is fixed 
	   * 	It can produce a new iterator with higher ind1 if it can bring lower
	   *  sum of values (cannot be equal as in the leftStream case) */
	  def rightStream(ind: Int, s1: Stream[ValuePairLeft],
	    s2: Stream[ValuePairRight]): Stream[ValuePairOut] = {
	    entering("rightStram", ind, s1, s2)
	
	    // store right element from right stream
	    val rightPair@(rightElem, rightWeight) = s2.head
	    val (leftElemHead, leftWeightHead) = s1.head
	
//	    val producePartial: ValuePairLeft => ValuePairOut =
//	      combineTwoValues(_, rightPair)
	
	    ( combine(leftElemHead, rightElem), leftWeightHead + rightWeight ) #:: {
	      if (!s1.tail.isEmpty && !s2.tail.isEmpty)
	      	iteratorsToBeAdded += rightStream(ind + 1, s1.tail, s2.tail).iterator.buffered
	    	s1.tail map  { p => combineTwoValuesLeft(p, rightElem, rightWeight) }
	    }
	  }
	
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

object BinaryStream {
	def apply[T, V, U](s1: Streamable[T], s2: Streamable[V])(combine: (T, V) => U) =
	  new BinaryStream(s1, s2)(combine)

	def memoized[T, V, U](s1: Streamable[T], s2: Streamable[V])(combine: (T, V) => U) =
	  new BinaryStream(s1, s2)(combine) with Memoized[U]
}