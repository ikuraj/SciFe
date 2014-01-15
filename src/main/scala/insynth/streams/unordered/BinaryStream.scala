package insynth.streams
package unordered

import insynth.streams.Streamable
import scala.collection.immutable.Stream.consWrapper

/**
 * binary stream takes elements from two streams and combines them into new elements, the progress
 * is made concurrently on both streams and no combination is repeated (as long as combine is not
 * commutative)
 * @param <T>
 * @param <U>
 * @param s1, s2 
 */
class BinaryStream[T, V, U](val s1: Streamable[T], val s2: Streamable[V])(combine: (T, V) => U) extends Streamable[U] {
    
  override def size =
    if (s1.size <= -1 || s2.size <= -1) -1
    else s1.size + s2.size

  // if one of the streams is infinite we will have an infinite stream
  lazy val isInfiniteFlag = s1.isInfinite || s2.isInfinite
  
  override def isInfinite = isInfiniteFlag
  
  // "left side" iterators
  lazy val lit1 = s1.getStream.iterator
  var lit2: Iterator[V] = _ // will be initialized in the leftStream
  // stored element of the first iterator
  var el1: T = _

  // stream value, store it in order to use memoization of previously computed elements
  lazy val leftStream = {
    // inner function which "produces" new elements
    def loop(index1: Int, index2: Int): Stream[U] = {
      // if first iterator is ahead of second and the second has next element
      if (index1 >= index2 && lit2.hasNext)
        // combine elements from 1st and 2nd iterator and loop with next element of 2nd iterator
        combine(el1, lit2.next) #:: loop(index1, index2 + 1)
      // otherwise, if 1st iterator has next element
      else if (lit1.hasNext) {
        // reset the second iterator
        lit2 = s2.getStream.iterator
        // get next element from 1st
        el1 = lit1.next
        // continue by increasing the 1st index
        loop(index1 + 1, 0)
      } else
        // we reached the end of the stream
        Stream.empty
    }

    // start looping from -1, 0 (will eventually call (0,0))
    loop(-1, 0)
  }
  
  var rit1: Iterator[T] = _
  var el2: V = _
  lazy val rit2 = s2.getStream.iterator

  // similar as left stream but now using different set of fields and starting with equal indices
  // and checking if 1st index is strictly lower than the 2nd
  lazy val rightStream = {
    def loop(index1: Int, index2: Int): Stream[U] = {
      if (index1 < index2 && rit1.hasNext)
        combine(rit1.next, el2) #:: loop(index1 + 1, index2)
      else if (rit2.hasNext) {
        rit1 = s1.getStream.iterator
        el2 = rit2.next
        loop(0, index2 + 1)
      } else
        Stream.empty
    }

    // we need to forward rit2 iterator since the first greater value should start from the second
    // (when indices are (0,1))
    if (rit2.hasNext) {
      rit2.next
      loop(0, 0)
    } else
      Stream.empty
  }
  
  //var index = -1
  
  // produces stream where left and right streams are alternated
  lazy val alternativeStream = {
	  def getStreamAlternative(s1: => Stream[U], s2: => Stream[U]): Stream[U] = 
  		if (s1.isEmpty) s2
		  else s1.head #:: getStreamAlternative(s2, s1.tail)
  		
    getStreamAlternative(leftStream, rightStream)    
  }
    
  override def getStream = alternativeStream
  
}

object BinaryStream {
	def apply[T, V, U](s1: Streamable[T], s2: Streamable[V])(combine: (T, V) => U) =
	  new BinaryStream(s1, s2)(combine)

	def memoized[T, V, U](s1: Streamable[T], s2: Streamable[V])(combine: (T, V) => U) =
	  new BinaryStream(s1, s2)(combine) with Memoized[U]
}