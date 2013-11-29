package insynth.streams.ordered

import scala.collection.mutable.{ ArrayBuffer, LinkedList => MutableList, HashMap => MutableMap, HashSet => MutableSet }

import insynth.util.logging._
import insynth.streams.{ OrderedStreamable => Streamable }

class BinaryStream[T, V, U](val s1: Streamable[T], val s2: Streamable[V])
	(combine: (T, V) => U)
	extends Streamable[U] with HasLogger {
  
  // if one of the streams is infinite we will have an infinite stream
  lazy val isInfiniteFlag = s1.isInfinite || s2.isInfinite
  
  override def isInfinite = isInfiniteFlag
  
  override def isDepleted: Boolean =
    s1.isDepleted || s2.isDepleted

  override def nextReady(ind: Int): Boolean = {
    fine("nextReady ind " + ind + " enumeratedSize " + enumeratedSize)
    assert(ind < enumeratedSize + 1, toString + " ind " + ind + " enumeratedSize " + enumeratedSize)
    val res = ind < enumeratedSize || hasNextFromIndexes || iterators.exists(_.hasNext)    
    // || getMinIterator(lastIndexCache) >= 0 //s1.nextReady && s2.nextReady
    fine("exiting nextReady ind " + ind + " ready " + res)
    res
  }
    
  var enumeratedSize = 0
      
  var iterators = ArrayBuffer[BufferedIterator[(U, Int)]]()
  
  var iteratorsToBeAdded = MutableList[BufferedIterator[(U, Int)]]()
  
  // (isLeft?, index) -> index
  var indexesToCheck: MutableMap[(Boolean, Int), Int] = MutableMap.empty
  // for checking when indexes are equal
  var indexesToCheckSet: MutableSet[Int] = MutableSet(0)
  
  def hasNextFromIndexes = {
    fine("indexesToCheck = " + indexesToCheck + " indexesToCheckSet = " + indexesToCheckSet)
    (false /: indexesToCheck) {
      (res, p) => {
        val isLeft = p._1._1        
        val ind = p._1._2
        val currentInd = p._2
        if (isLeft) {
          info("Left stream (ind, ind) = " + (ind, currentInd) + ", is ready =" + s2.nextReady(currentInd))
          res || s2.nextReady(currentInd)
        }
        else {
          info("Right stream (ind, ind) = " + (currentInd, ind) + ", is ready =" + s1.nextReady(currentInd))
          res || s1.nextReady(currentInd)
        }
      }
    } || 
    (false /: indexesToCheckSet) {
      (res, ind) => {
        res || (s1.nextReady(ind) && s2.nextReady(ind))
      }
    }
  }
  
  def addToIterators(it: => BufferedIterator[(U, Int)], isLeft: Boolean, ind: Int) = {//iterators append it    
    info("adding iterator " + (isLeft: Boolean, ind: Int))
    indexesToCheck += ((isLeft, ind) -> (ind + 1))
    if (isLeft) {
      indexesToCheckSet += (ind + 1)
    } 
    iteratorsToBeAdded :+= it
  }
  
  def addToIterators(it: BufferedIterator[(U, Int)]) = {//iterators append it    
    iteratorsToBeAdded :+= it
  }
  
//  var leftStreamsMap: MutableMap[Int, Stream[(U, Int)]] = MutableMap.empty
//  var rightStreamsMap: MutableMap[Int, Stream[(U, Int)]] = MutableMap.empty
  
  /** Stream that streams over combinations of two streams with indexes ind1, ind2 such that
   *  ind2 > ind1 at all times and ind1 is fixed 
   * 	It can produce a new iterator with higher ind1 if it can bring lower
   *  sum of values */
  def leftStream(ind1: Int): Stream[(U, Int)] = {
    info("constructing leftStream with : " + ind1)
    
//    if (leftStreamsMap.contains(ind1)) {
//      info("returning from cache")
//      return leftStreamsMap(ind1)
//    }
    
    // drop unused first parts of streams
    lazy val leftStreamPart = s1.getStream.drop(ind1)
    lazy val leftValuesPart = s1.getValues.drop(ind1)
    
    // index for start iterating on s2
    val startingInd2 = ind1 + 1
    // iterator over second stream
    lazy val rightIterator = (s2.getStream zip s2.getValues).drop(startingInd2).iterator.buffered
    
    // flag if new stream was already created and added
    // NOTE needs to be computed right away because of the check in loop method
    var produced = false
    
    lazy val producingPossible =
      leftValuesPart match {
      	case _ #:: secondValue #:: _ => true
      	case _ =>
      	  // prevent checking the condition
      	  false
    	}
    
    // store first element from first stream
    lazy val leftElem = leftStreamPart.head
    lazy val leftValue = leftValuesPart.head
    // value of the least sum with ind1 + 1
    // this is checked to see if new iterator should be created
    lazy val nextLeftSizeCombined = 
      leftValuesPart match {
      	case _ #:: secondValue #:: _ => secondValue + s2.getValues(startingInd2)
      	case _ =>
      	  Int.MaxValue 
    	}        
    
    //info("nextLeftSizeCombined : " + nextLeftSizeCombined)
                
    def loop(ind2: Int): Stream[(U, Int)] = {
      fine("leftStream loop: " + ind2 + " (rightIterator.hasNext, prouced)=" +
        (rightIterator.hasNext, produced))
      
      if (rightIterator.hasNext) {
	      val (nextRightElem, nextRightSize) = rightIterator.head
	      val nextSize = leftValue + nextRightSize
	      
	      if ( !produced && (ind2 - ind1 > 1) && producingPossible && nextSize > nextLeftSizeCombined )
	      {
		      // add a new iterator
		      addToIterators( leftStream(ind1 + 1).iterator.buffered, true, ind1 + 1 )	   
		      // set the flag
		      produced = true
		      
	        rightIterator.next
	        assert(indexesToCheck.contains((true, ind1)), "could not find " +
	        		(true, ind1) + " in indexesToCheck=" + indexesToCheck)
		      indexesToCheck.update((true, ind1), indexesToCheck(true, ind1) + 1)
	        
	        (combine(leftStreamPart(1), s2.getStream(startingInd2)), nextLeftSizeCombined) #::
	        (combine(leftElem, nextRightElem), nextSize) #:: loop(ind2 + 1)
	      } else {
	        rightIterator.next
	        assert(indexesToCheck.contains((true, ind1)), "could not find " +
	        		(true, ind1) + " in indexesToCheck=" + indexesToCheck)
		      indexesToCheck.update((true, ind1), indexesToCheck(true, ind1) + 1)
	        (combine(leftElem, nextRightElem), nextSize) #:: loop(ind2 + 1)
	      }
      }
      // if stream did not produce new iterator and the difference between indexes is >=2
      // (otherwise it may spawn iterators even if there is no next element)        
      else if (!produced && ind2 - ind1 > 1 && producingPossible) {
        if (ind2 - ind1 > 2) {
		      // add a new iterator
		      addToIterators( leftStream(ind1 + 1).iterator.buffered, true, ind1 + 1 )	   
		      // set the flag
		      produced = true
        }
	      
	      // return "equal" combination
        indexesToCheckSet -= startingInd2
	      Stream(
	        (combine(leftStreamPart(1), s2.getStream(startingInd2)),
	        nextLeftSizeCombined)
	      )      
      } else
        Stream.empty      
    }
    
//    val resultStream = 
//	    if (leftValuesPart.isEmpty)
//	      Stream.empty
//	    else	
//	    	loop(startingInd2)
//	    	
//  	assert(!leftStreamsMap.contains(ind1), "!leftStreamsMap.contains(ind1)")
//  	leftStreamsMap += ind1 -> resultStream
//  	resultStream
    if (leftValuesPart.isEmpty)
      Stream.empty
    else	
    	loop(startingInd2)
  }
  
  /** Stream that streams over combinations of two streams with indexes ind1, ind2 such that
   *  ind2 < ind1 at all times and ind1 is fixed 
   * 	It can produce a new iterator with higher ind1 if it can bring lower
   *  sum of values (cannot be equal as in the leftStream case) */
  def rightStream(ind2: Int): Stream[(U, Int)] = {
    info("constructing rightStream with : " + ind2)
        
    // drop unused first parts of streams
    lazy val rightStreamPart = s2.getStream.drop(ind2)
    lazy val rightValuesPart = s2.getValues.drop(ind2)
    
    // index for start iterating on s2
    val startingInd1 = ind2 + 1
    // iterator over second stream
    lazy val leftIterator = (s1.getStream zip s1.getValues).drop(startingInd1).iterator.buffered
    
    // flag if new stream was already created and added
    // NOTE needs to be computed right away because of the check in loop method
    var produced = false
      
    lazy val producingPossible =
      rightValuesPart match {
      	case _ #:: secondValue #:: _ => true
      	case _ =>
      	  // prevent checking the condition
      	  false
    	}
    
    // store first element from first stream
    lazy val rightElem = rightStreamPart.head
    lazy val rightValue = rightValuesPart.head
    // value of the least sum with ind2 + 1
    // this is checked to see if new iterator should be created
//    lazy val nextRightSizeCombined = 
//      rightValuesPart match {
//      	// caller ensures that there actually is s1.getValues(startingInd1 + 1)
//      	case _ #:: secondValue #:: _  => secondValue + s1.getValues(startingInd1 + 1)
//      	case _ =>
//      	  Int.MaxValue 
//    	}        
            	
    // produce the pair and new iterator
    def produce = {
      // add a new iterator
      addToIterators( rightStream(ind2 + 1).iterator.buffered, false, ind2 + 1 )	   
      // set the flag
      produced = true
      
      // return new combination (its no equal in indexes)
//      (combine(s1.getStream(startingInd1 + 1), rightStreamPart(1)), nextRightSizeCombined)      
    }

    def loop(ind1: Int): Stream[(U, Int)] = {
      if (leftIterator.hasNext) {
	      val (nextLeftElem, nextLeftValue) = leftIterator.head
	      val nextSize = rightValue + nextLeftValue
	      
//	      if (!produced && ind1 - ind2 > 2 && nextSize > nextRightSizeCombined ) {
//	        produce #:: loop(ind1)
//	      } else {
//	        leftIterator.next	        
//	        (combine(nextLeftElem, rightElem), nextSize) #:: loop(ind1 + 1)
//	      }
	      
	      // if the difference is >=2 then add the new stream (when it becomes 3) that one should
	      // also be checked
	      // NOTE each right stream can produce only one rightStream
	      if (!produced && ind1 - ind2 > 1 && producingPossible) {
	        info("producing right, (ind1, ind2) is: " + (ind1, ind2))
	        produce
	      }

        leftIterator.next	        
	        assert(indexesToCheck.contains(false, ind2), "could not find " +
	        		(false, ind2) + " in indexesToCheck=" + indexesToCheck)
		      indexesToCheck.update((false, ind2), indexesToCheck(false, ind2) + 1)
        (combine(nextLeftElem, rightElem), nextSize) #:: loop(ind1 + 1)	      
      } 
      else
        Stream.empty      
    }
    
    if (rightValuesPart.isEmpty)
      Stream.empty
    else	
    	loop(startingInd1)
  }
	  
  // TODO this may return index of newly created iterator which will be favored?
  // case: last iterator creates a new iterator so the next time that one is first
  private def getMinIterator(lastIndex: Int) = {
    entering("getMinIterator", lastIndex.toString)
    info("iterators.size: " + iterators.size)
        
    var minInd = -1//(lastIndex + 1) % iterators.size
    var minValue = Int.MaxValue
    //var minIt: BufferedIterator[(U, Int)] = iterators(minInd)
            
    iterators appendAll iteratorsToBeAdded
    iteratorsToBeAdded = MutableList.empty
    
    var ind = 0
    while(ind < iterators.size) {
      val indToCheck = (lastIndex + ind) % iterators.size
      info("checking index: " + indToCheck)
      if (iterators(indToCheck).hasNext && iterators(indToCheck).head._2 < minValue ) {
        minInd = indToCheck
        minValue = iterators(indToCheck).head._2
      }
      ind += 1
    }
        
    //iterators.zipWithIndex.minBy( _._1.head._2 )
    exiting("getMinIterator", minInd.toString)
    
    minInd
    //(minInd, (minInd + 1) % iterators.size)
  }
  
  // produces stream where left and right streams are alternated
  // NOTE that it is fair
  lazy val alternativeStream = {
    info("alternative stream: " + (s1.getValues.isEmpty, s2.getValues.isEmpty,
      s1.getStream.isEmpty, s2.getStream.isEmpty))
    
    // NOTE deal with fairness
    // index to store which iterator was min the last
    def loop(lastIndex: Int): Stream[(U, Int)] = {
      
      //val (minInd, indNext) = getMinIterator(lastIndex)
      val minInd = getMinIterator(lastIndex)
      
      if (minInd >= 0) {
      	val res = iterators(minInd).next #:: loop(minInd + 1)
//  			lastIndexCache = minInd + 1
      	enumeratedSize += 1
      	res
//        {
//        	if (iteratorsToBeAdded.size > 0)
//        	  loop(0)
//      	  else
//      	  	loop(indNext)
//      	}
      }
    	else
    	  Stream.empty
    }
    
//    lastIndexCache = 0
  	enumeratedSize += 1  	
    indexesToCheck += ((true, 0) -> 1)
    indexesToCheck += ((false, 0) -> 1)
    indexesToCheckSet -= 0
    
	  (combine(s1.getStream.head, s2.getStream.head), s1.getValues.head + s2.getValues.head) #:: {
	    addToIterators( leftStream(0).iterator.buffered )
	    addToIterators( rightStream(0).iterator.buffered )
	    loop(0)
	  }
  }
    
  override def getStream =
    if (s1.getValues.isEmpty || s2.getValues.isEmpty)
      Stream.empty
    else
      alternativeStream map { _._1 }
  
  override def getValues = 
    if (s1.getValues.isEmpty || s2.getValues.isEmpty)
      Stream.empty
    else
      alternativeStream map { _._2 }
  
}

object BinaryStream {
	def apply[T, V, U](s1: Streamable[T], s2: Streamable[V])(combine: (T, V) => U) =
	  new BinaryStream(s1, s2)(combine)
}
//  			    
//    leftStream.head #:: getStreamAlternative(rightStream, leftStream.tail) 
//  }
//    
//  override def getStream = {
//    entering(this.getClass.getName, "getStream:" + getName)
//    alternativeStream map { _._1 }
//  }
//  
//  override def getValues = alternativeStream map { _._2 }
//  
//}

//class BinaryStream[T, V, U](val s1: Streamable[T], val s2: Streamable[V])
//	(combine: (T, V) => U)
//	extends Streamable[U] with HasLogger {
//    
//  // NOTE since we are accessing streams, this should be lazy (since we can have InitializingRRs)
//  val innerBinaryStream = 
//    DecomposingBinaryStream[(T, Int), (V, Int), (U, Int)](
//      org.SingleStream(s1.getStream zip s1.getValues, s1.isInfinite),
//      org.SingleStream(s2.getStream zip s2.getValues, s2.isInfinite)
//	  )	{
//    	case ( (tElement, tValue), (vElement, vValue)) =>
//    	  (combine(tElement, vElement), tValue + vValue)
//  	}
//  
//  // if one of the streams is infinite we will have an infinite stream
//  lazy val isInfiniteFlag = s1.isInfinite || s2.isInfinite
//  
//  override def isInfinite = isInfiniteFlag
//      
//  def leftStream = innerBinaryStream.leftStream
//  def rightStream = innerBinaryStream.rightStream
//  def middleStream = innerBinaryStream.middleStream
//	  
//  // produces stream where left and right streams are alternated
//  // NOTE that it is fair
//  lazy val alternativeStream = {
//	  def getStreamAlternativeFor2(s1: Stream[(U, Int)], s2: Stream[(U, Int)]): Stream[(U, Int)] = {
//  		if (s1.isEmpty) s2
//  		if (s2.isEmpty) s1
//		  else if (s1.head._2 <= s2.head._2)
//	      s1.head #:: getStreamAlternativeFor2(s2, s1.tail)
//      else
//        s2.head #:: getStreamAlternativeFor2(s1, s2.tail)
//	  }
//    
//	  def getStreamAlternativeFor3(streams: List[Stream[(U, Int)]]): Stream[(U, Int)] = {
//	    streams.filterNot( _.isEmpty ) match {
//	      case Nil => Stream.empty
//	      case List(xs) => xs
//	      case xs1 :: xs2 :: Nil =>
//				  if (xs1.head._2 <= xs2.head._2)
//			      xs1.head #:: getStreamAlternativeFor2(xs2, xs1.tail)
//		      else
//		        xs2.head #:: getStreamAlternativeFor2(xs1, xs2.tail)
//	      case xs1 :: xs2 :: xs3 :: Nil =>
//        	if (xs1.head._2 <= xs2.head._2) { 
//        	  
//        	  if (xs1.head._2 <= xs3.head._2)
//        	    xs1.head #:: getStreamAlternativeFor3(xs2 :: xs3 :: xs1.tail :: Nil)
//        	  else
//        	    xs3.head #:: getStreamAlternativeFor3(xs1 :: xs2 :: xs3.tail :: Nil)
//        	    
//        	} else { 
//        	  
//        	  if (xs2.head._2 <= xs3.head._2) 
//        	    xs2.head #:: getStreamAlternativeFor3(xs1 :: xs3 :: xs2.tail :: Nil)
//        	  else
//        	    xs3.head #:: getStreamAlternativeFor3(xs1 :: xs2 :: xs3.tail :: Nil)
//        	    
//        	}
////	        val listMin = list.minBy( _.head._2 )
////	        
////	        lazy val listModified =
////		        for (stream <- list) yield {
////		          if (stream == listMin)
////		            stream.tail
////	            else 
////	            	stream
////		        }
////	        
////	        listMin.head #:: getStreamAlternativeFor3(listModified.last :: listModified.init)
//	    }
//    }
//  			    
//    middleStream.head #:: getStreamAlternativeFor3(List(leftStream, rightStream, middleStream.tail)) 
//  }
//    
//  override def getStream = {
//    entering(this.getClass.getName, "getStream:" + getName)
//    alternativeStream map { _._1 }
//  }
//  
//  override def getValues = alternativeStream map { _._2 }
//  
//}