package insynth.streams.unordered

import scala.collection.mutable.{ LinkedList => MutableList }

import insynth.streams.Streamable

// can specialize for infinity to be more efficient
/**
 * round robin takes elements of parameter streams concurrently
 * @param <T>
 * @param streams stream that form a new stream
 */
class RoundRobbin[T](val streams: Seq[Streamable[T]]) extends Streamable[T] {
  
  override def size =
    if (streams.exists(_.size <= 1)) -1
    else streams.map(_.size).sum
    
  // this stream is infinite if there is an infinite stream in the array
  // compute infinite flag only once
  lazy val isInfiniteFlag = //streams.exists( _.isInfinite )
  {
    var found = false
    var ind = 0
    while (!found && ind < streams.size) {
      found = streams(ind).isInfinite
        
      ind += 1
    }
    found
  }
  
  override def isInfinite = isInfiniteFlag
  
  // iterators that track the positions in each stream
  // hidden so it looks as functional
  // TODO make this an array
  protected var _iterators: MutableList[Iterator[T]] = MutableList.fill(streams.size)(null)
  // NOTE cannot do this because InitializingRoundRobbin is constructing RoundRobbin and it may
  // not have eager evaluations
  //private lazy val _iterators: Seq[Iterator[T]] = streams map { _.getStream.iterator }
    
  protected def iterators(ind: Int) = {
    if (_iterators(ind) == null) {
      _iterators(ind) = streams(ind).getStream.iterator
    }
    _iterators(ind)
  }
  
  protected def oneWithNext(currentInd: Int): Int = {
    //(false /: iterators) { (res, it) => res || it.hasNext }
    var ind = 0
    while (ind < _iterators.size) {
      val indToCheck = (currentInd + ind) % _iterators.size
      val elIterator = iterators( indToCheck )
      if (elIterator.hasNext)
        return indToCheck
        
      ind += 1
    }
    
    -1
  }
  
  // stream value, store it in order to use memoization of previously computed elements
  protected lazy val stream = {      
    // inner function which "produces" new elements
    def loop(index: Int): Stream[T] = {  		
  		// NOTE would val = oneWithNext here increase the stack unecesarily?
  		val nextIndex = oneWithNext(index)
      // check if there is at least one iterator with next element
  		if ( nextIndex > -1 ) {
//  		  // if the current iterator has next element
//  		  if (iterators(index).hasNext) {
  		    // prepend the element to a recursively computer stream
  		  	iterators(nextIndex).next #:: loop((nextIndex + 1) % streams.size)
//  		  }
//  		    //Stream.cons(iterators(index).next, loop((index + 1) % streams.size))
//		  	else {		  	  
//  		    logger.info("iterators(index).hasNext was false")
//		  	  // current iterator has reached end, continue with next one
//		  	  loop((index + 1) % streams.size)
//		  	}
		  }
			else
			  // no iterator has next, return empty stream
			  Stream.empty
    }
			  
	  // start with first iterator
    loop(0)    
  }
    
  override def getStream = stream
}

object RoundRobbin {
  def apply[T](streams: Seq[Streamable[T]]) = new RoundRobbin(streams)
}