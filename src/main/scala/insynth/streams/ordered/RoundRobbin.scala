package insynth.streams.ordered

import scala.collection.mutable.{ Seq => MutableSeq }

import insynth.util.logging._
import insynth.streams.{ OrderedStreamable => Streamable }
import insynth.streams.unordered.{ RoundRobbin => UnRoundRobbin }

class RoundRobbin[T] protected[streams] (val streams: IndexedSeq[Streamable[T]])
	extends Streamable[T] with HasLogger {
  
  override def isInfinite = isInfiniteFlag
  
  override def isDepleted: Boolean = throw new RuntimeException//getNextIndex._2 == -1 // wtv
  override def nextReady(ind: Int): Boolean = {
    assert(ind <= enumeratedCounter + 1, "ind=" + ind + ", enumeratedCounter=" + enumeratedCounter)
    if (ind <= enumeratedCounter) true
    else {
	    val res = if (ind != enumeratingCounter) {
	      enumeratingCounter = ind
		    val resIn = ((false /: (streams zip iteratorIndexes)) {
		      (res, p) => res || p._1.nextReady(p._2)
		    })
		    enumeratingCounter = -1
		    resIn
	    } else false
	    fine("ready for " + ind + "? " + res)
	    res
    }
  }
    //streams.exists(! _.nextReady)
  
  // TODO merge into one counter
  var enumeratedCounter = -1
  // dealing with loops
  var enumeratingCounter = -1
  
  // iterators that track the positions in each stream
  // hidden so it looks as functional
  private var _valueIterators: Array[BufferedIterator[Int]] = Array.fill(streams.size)(null)
  protected var _iterators: Array[Iterator[T]] = Array.fill(streams.size)(null)
  protected var iteratorIndexes: Array[Int] = Array.fill(streams.size)(0)
  
  protected def iterators(ind: Int) = {
    if (_iterators(ind) == null) {
      _iterators(ind) = streams(ind).getStream.iterator
    }
    _iterators(ind)
  }
  protected def valueIterators(ind: Int) = {
    if (_valueIterators(ind) == null) {
      _valueIterators(ind) = streams(ind).getValues.iterator.buffered 
    }
    _valueIterators(ind)
  }
    
  val valueIteratorsSize = streams.size
  
  var currentInd = 0
  
  // NOTE keeps fairness
  private def getNextIndex = {
    //entering("getNextIndex", currentInd.toString)
    var min = Int.MaxValue
    var minInd = -1
    var ind = 0
    while (ind < valueIteratorsSize) {
      val indToCheck = (currentInd + ind) % valueIteratorsSize
      
      fine("checking ready for indToCheck " + indToCheck + " for iteratorIndexes(indToCheck) "
        + iteratorIndexes(indToCheck) + " and is "
        + streams(indToCheck).nextReady(iteratorIndexes(indToCheck))
      )
      if (streams(indToCheck).nextReady(iteratorIndexes(indToCheck))) {
        assert(valueIterators(indToCheck).hasNext, "valueIterators(indToCheck).hasNext for " + indToCheck)
	      fine("checking index: " + indToCheck + ", valueIterators(indToCheck).hasNext: " + valueIterators(indToCheck).hasNext)
	      if (valueIterators(indToCheck).hasNext) fine("valueIterators(indToCheck).head: " + valueIterators(indToCheck).head)
	      if (valueIterators(indToCheck).hasNext && valueIterators(indToCheck).head < min) {
	      	fine("index passed: " + indToCheck + ", value: " + valueIterators(indToCheck).head)
	      	min = valueIterators(indToCheck).head
	  			minInd = indToCheck
	      }
      }
        
      ind += 1
    }
    
    exiting("getNextIndex", (min, minInd).toString)
    (min, minInd)
  }
  
  // stream value, store it in order to use memoization of previously computed elements
  protected lazy val streamWithValues = { 
    // inner function which "produces" new elements
    // TODO wow a bug found due to the name!?
    def loopXXX(index: Int): Stream[(T, Int)] = {
  		//entering(this.getClass.getName, "loop:" + this.getName, index)
      
      enumeratingCounter = enumeratedCounter + 1
  		fine("loopXXX for enumeratingCounter " + enumeratingCounter + " array:" + iteratorIndexes.mkString("(",",",")"))
  		
      currentInd = index
      // check if there is at least one iterator with next element
  		val res =
			  getNextIndex match {
	  		  case (nextValue, nextIndex) if nextIndex > -1 =>
	  		    iteratorIndexes(nextIndex) += 1
	  		    // forward the value iterator
	  		    valueIterators(nextIndex).next
				    // prepend the element to a recursively computed stream
				  	(iterators(nextIndex).next, nextValue) #:: loopXXX((index + 1) % streams.size)
	  		  case _ =>
					  // no iterator has next, return empty stream
					  Stream.empty  		    
	  		}
      
      enumeratedCounter += 1
      enumeratingCounter = -1
      res
    }
			  
	  // start with first iterator
    loopXXX(0)
  }
  
  override def getStream = {
//    fine("getStream RoundRobbin array:" + iteratorIndexes.mkString("(",",",")"))
    streamWithValues map { _._1 }
  }
  
  override def getValues = {
//    fine("getValues LazyRoundRobbin")
    streamWithValues map { _._2 }
  }
    
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
}

object RoundRobbin {
  def apply[T](streamsIn: => IndexedSeq[Streamable[T]]) = {
//    assert(streams.forall(!_.isInfinite))
//  	val streams = streamsIn.sortWith(!_.isInfinite && _.isInfinite)
  	
    new RoundRobbin(streamsIn)
  }
}

//class InitializingRoundRobin[T](val initStreams: List[Streamable[T]])
//	extends LazyStreamable[T] with HasLogger {
//  if (initStreams.isEmpty)
//    println("creating initStreams is empty!!!")
//  
//  var initialized = false
//      
//  var streams: List[Streamable[T]] = List.empty
//    
//  var innerRoundRobbin: RoundRobbin[T] = _
//  
//  override def addStreamable(s: Streamable[T]) = streams :+= s
//  
//  override def isInfinite = 
//    if (initialized) initStreams.exists( _.isInfinite )//innerRoundRobbin.isInfinite
//    else false
//      
//  private def getMinIndex = {
//    
//		val valueIterators = initStreams map { _.getValues.iterator.buffered }
//    
//    var min = Int.MaxValue
//    var minInd = -1
//    var ind = 0
//    while (ind < valueIterators.size) {
//      val indToCheck = ind % valueIterators.size
//      
//      if (valueIterators(indToCheck).hasNext && valueIterators(indToCheck).head < min) {
//      	min = valueIterators(indToCheck).head
//  			minInd = indToCheck
//      }        
//        
//      ind += 1
//    }
//    
//    //assert(minInd > -1, "minInd > -1")
//    (min, minInd)
//  }
//  
//  lazy val (minValue, minInd) = getMinIndex
//  
//  lazy val mappedInitStreams = initStreams.zipWithIndex map {
//    p =>
//    	if (p._2 == minInd)
//    	  SingleStream((p._1.getStream zip p._1.getValues).tail, p._1.isInfinite)
//  	  else p._1
//  }
//    
//  private def produceRoundRobbin = {
//    if (innerRoundRobbin == null)
//    	innerRoundRobbin = RoundRobbin[T]((mappedInitStreams ++ streams).toSeq)
//  	innerRoundRobbin
//  } 
//  
//  override def initialize = {    
//    // important to first initialize
//    // NOT??
//    //produceRoundRobbin
//    initialized = true
//  }
//    
//  lazy val stream = 
//    if (minInd > -1) initStreams(minInd).getStream.head #:: produceRoundRobbin.getStream
//    else Stream.empty    
//  
//  override def getStream = {
//    entering(this.getClass.getName + getName, "getStream")
//    info("initialized " + initialized)
//    
//    if (initialized) stream
//    else Stream.Empty
//  }
//  
//  override def getValues = 
//    if (initialized && minInd > -1) {
//      assert(minInd > -1)
//      minValue #:: produceRoundRobbin.getValues
//    }
//    else Stream.Empty
//}
//
//object InitializingRoundRobin {
//	def apply[T](initStreams: List[Streamable[T]]) = new InitializingRoundRobin(initStreams)
//}