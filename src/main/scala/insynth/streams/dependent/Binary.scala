package insynth.streams
package dependent

import scala.collection.mutable.{ ArrayBuffer, MutableList }
import insynth.util.logging._
import scala.language.postfixOps
import scala.annotation._
import insynth.streams.light.Finite

// TODO refactor ME
class BinaryFinite[I, O]
  (s1: light.Finite[I], s2: Dependent[I, O])
  extends light.Finite[O] with HasLogger {
  
  val rr = { 
    val rightStreams = 
      for (ind <- 0 until s1.size; stream = s2.getStream(s1(ind)); if stream.size > 0 )
        yield stream
        
    light.RoundRobbinFinite.fixed[O](
      Array(rightStreams: _*)
    )
  }
  
  override def size = rr.size
  
  override def apply(ind: Int) =
    rr(ind)
  
}

class BinaryFiniteChain[I, I2, O]
  (s1: light.Finite[I], s2: Dependent[I2, O])(chain: I => I2)
  extends light.Finite[O] with HasLogger {
  
  val rr = { 
    val rightStreams = 
      for (ind <- 0 until s1.size; stream = s2.getStream( chain(s1(ind)) ); if stream.size > 0 )
        yield stream
        
    light.RoundRobbinFinite.fixed[O](
      Array(rightStreams: _*)
    )
  }
  
  override def size = rr.size
  
  override def apply(ind: Int) =
    rr(ind)
  
}

class BinaryFiniteCombine[I, O, R]
  (s1: light.Finite[I], s2: Dependent[I, O], combine: (I, O) => R)
  extends light.Finite[R] with HasLogger {
  
  val rr = {
    val streams = 
      for (ind <- 0 until s1.size; leftProduced = s1(ind); rightStream = s2.getStream( leftProduced );
        if rightStream.size > 0 ) yield {
          light.Mapper( rightStream, { (rightProduced: O) => combine(leftProduced, rightProduced) })
        }

    light.RoundRobbinFinite.fixed[R](  
      Array(streams: _*)
    )
  }
  
  override def size = rr.size
  
  override def apply(ind: Int) =
    rr(ind)
  
}

class BinaryFiniteChainCombine[I, I2, O, R]
  (s1: light.Finite[I], s2: Dependent[I2, O], chain: I => I2, combine: (I, O) => R)
  extends light.Finite[R] with HasLogger {
  
  val rr = {
    val streams = 
      for (ind <- 0 until s1.size; leftProduced = s1(ind); rightStream = s2.getStream( chain(leftProduced) );
        if rightStream.size > 0 ) yield {
          light.Mapper( rightStream, { (rightProduced: O) => combine(leftProduced, rightProduced) })
        }

    light.RoundRobbinFinite.fixed[R](  
      Array(streams: _*)
    )
  }
  
  override def size = rr.size
  
  override def apply(ind: Int) =
    rr(ind)
  
}

// NOTE this only works if all dependent streams are finite
case class Binary[I, I1, O]
  (s1: Dependent[I, I1], s2: Dependent[I1, O])
  extends Dependent[I, O] {
  
  def getStream(parameter: I) =
    BinaryFinite(s1.getStream(parameter), s2)
    
}

object BinaryFinite {
  
  def apply[I, O](s1: light.Enumerable[I], s2: Dependent[I, O]) = {
    s1 match {
      case f: light.Finite[I] =>
        new BinaryFinite(f, s2)
      case _ => throw new RuntimeException
    }    
  }
  
  def chain[I, I2, O](s1: light.Enumerable[I], s2: Dependent[I2, O])(chain: I => I2) = {
    s1 match {
      case f: light.Finite[I] =>
        new BinaryFiniteChain(f, s2)(chain)
      case _ => throw new RuntimeException
    }    
  }
  
  def chainCombined[I, I2, O, R](s1: light.Enumerable[I], s2: Dependent[I2, O],
    chain: I => I2, combine: (I, O) => R) = {
    s1 match {
      case f: light.Finite[I] =>
        new BinaryFiniteChainCombine(f, s2, chain, combine)
      case _ => throw new RuntimeException
    }    
  }
  
}

object BinaryFiniteMemoized {
  
  def apply[I, O](s1: light.Enumerable[I], s2: Dependent[I, O]) = {
    s1 match {
      case f: light.Finite[I] =>
        new BinaryFinite(f, s2) with light.Memoized[O]
      case _ => throw new RuntimeException
    }    
  }
  
  def chain[I, I2, O](s1: light.Enumerable[I], s2: Dependent[I2, O])(chain: I => I2) = {
    s1 match {
      case f: light.Finite[I] =>
        new BinaryFiniteChain(f, s2)(chain) with light.Memoized[O]
      case _ => throw new RuntimeException
    }    
  }
  
  def combine[I, O, R](s1: light.Enumerable[I], s2: Dependent[I, O], combine: (I, O) => R) = {
    s1 match {
      case f: light.Finite[I] =>
        new BinaryFiniteCombine(f, s2, combine) with light.Memoized[R]
      case _ => throw new RuntimeException
    }    
  }
  
  def chainCombined[I, I2, O, R](s1: light.Enumerable[I], s2: Dependent[I2, O],
    chain: I => I2, combine: (I, O) => R) = {
    s1 match {
      case f: light.Finite[I] =>
        new BinaryFiniteChainCombine(f, s2, chain, combine) with light.Memoized[R]
      case _ => throw new RuntimeException
    }    
  }
  
}

//case class Binary[I, O1, I2, O2, O]
//  (s1: Dependent[I, O1], s2: Dependent[I2, O2])
//  (chain: O1 => I2) (combine: (O1, O2) => O) extends Dependent[I, O] with HasLogger {
//  
//  override def getStream(parameter: I) = {      
//    entering("getStream", parameter)
//    val leftStream = s1.getStream(parameter)
//    new InnerBinaryStream(leftStream) getNext 0
//  }
//	  
//	private[Binary] class InnerBinaryStream(var s1: Stream[O1]) extends HasLogger {
//    type ValuePairLeft = O1
//    type ValuePairRight = O2
//    type ValuePairOut = O
//	  
//	  /**
//	   * Returns index of the iterator with minimal weight
//	   * NOTE: fairness is guaranteed by keeping track of which iterator was forwarded previously
//	   * @param lastIndex index of previously forwarded iterator
//	   * @return index of the iterator with minimal weight
//	   */
//	  final def getNext(lastIndex: Int): Stream[ValuePairOut] = {
//	    entering("getNext", lastIndex)
//	    
//	    val nextIndex =
//	      if( lastIndex + 1 >= iterators.size ) {
//	        iterators append newLeftStream.iterator.buffered
//	        // NOTE: no need to check we assume streams are infinite
//	        s1 = s1.tail
//	        0
//	      } else
//	        lastIndex + 1
//	            
//      iterators(nextIndex).next #:: getNext(nextIndex)
//	  }
//	  
//	  /** Stream that streams over combinations of two streams with indexes ind1, ind2 such that
//	   *  ind2 >= ind1 at all times and ind1 is fixed 
//	   * 	It can add a new iterator with left index equal to ind1 + 1 if it has next lowest
//	   *  sum of values */
//	  def newLeftStream: Stream[ValuePairOut] = {
//	    entering("newLeftStream", s1)
//	    
//	    // store first element from first stream
//	    val leftElem = s1.head
//	    val rightStream = s2.getStream( chain(leftElem) )
//	      
//	    rightStream map { p => combine(leftElem, p) }
//	  }
//	  
////	  /** Stream that streams over combinations of two streams with indexes ind1, ind2 such that
////	   *  ind2 < ind1 at all times and ind1 is fixed 
////	   * 	It can produce a new iterator with higher ind1 if it can bring lower
////	   *  sum of values (cannot be equal as in the leftStream case) */
////	  def rightStream(ind: Int, s1: Stream[ValuePairLeft],
////	    s2: Stream[ValuePairRight]): Stream[ValuePairOut] = {
////	    entering("rightStram", ind, s1, s2)
////	
////	    // store right element from right stream
////	    val rightHead = s2.head
////	    val leftHead = s1.head
////	
//////	    val producePartial: ValuePairLeft => ValuePairOut =
//////	      combineTwoValues(_, rightPair)
////	
////	    combine(leftHead, rightHead) #:: {
////	      if (!s1.tail.isEmpty && !s2.tail.isEmpty)
////	      	iteratorsToBeAdded += rightStream(ind + 1, s1.tail, s2.tail).iterator.buffered
////	    	s1.tail map  { p => combine(p, rightHead) }
////	    }
////	  }
//	
//	  val INITIAL_ARRAYBUF_SIZE = 16
//	  // set of iterators checked for next value
//	  var iterators = new ArrayBuffer[BufferedIterator[ValuePairOut]](INITIAL_ARRAYBUF_SIZE)
//	  // shadow copy for fast swapping
//	  var iterators_shadow = new ArrayBuffer[BufferedIterator[ValuePairOut]](INITIAL_ARRAYBUF_SIZE)
//	  
//	  private def swapIterators = {
//	    val tmp = iterators
//	    iterators = iterators_shadow
//	    iterators_shadow = tmp
//	    iterators_shadow.clear
//	  }
//	  
//	  // iterators to be added in the next iteration
//	  var iteratorsToBeAdded = MutableList[BufferedIterator[ValuePairOut]]()
//	  
//	  def addToIterators(it: BufferedIterator[ValuePairOut]) = {
//	    iteratorsToBeAdded += it
//	  }
//	  
//	}
//  
//}