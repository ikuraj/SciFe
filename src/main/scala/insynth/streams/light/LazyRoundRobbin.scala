//package insynth.streams.ordered
//
//import scala.collection.mutable
//
//import insynth.streams._
//
//import insynth.util.logging._
//
///**
// * LazyRoundRobbin allows adding Streamables after creation. Added streamables may
// * form recursive links in the structure of the constraint graph. That is why
// * elements from the added streamables are enumerated always after at least one
// * element from initStreams. 
// * NOTE: all addded streamables are considered to be infinite
// */
//class LazyRoundRobbin[T](val initStreams: Seq[IntegerWeightStreamable[T]])
//	extends IntegerWeightStreamable[T] with AddStreamable[T] with HasLogger with CountedLogger {
//  require(initStreams.size > 0)
//  
//  // can be computed once for all getValuedStream calls
//  // NOTE: do not save streams into a val because it will memoize
////  lazy val (minHead, minInd) = initStreams.map(_.getValuedStream.head).zipWithIndex.minBy(_._1._2)
//  
//  // NOTE: OK we're losing the "lazy" and enforcing head computations... (since Scala does not know how to do this
//  // without saving RHS in a field)
//  // NOTE: this does not work also?!
////  val (minHead, minInd) = initStreams.map(_.getValuedStream.head).zipWithIndex.minBy(_._1._2)
//
//  // NOTE: working but invokes 2x per getStream call
////  def minHead = initStreams.map(_.getValuedStream.head).zipWithIndex.minBy(_._1._2)._1
////  def minInd = initStreams.map(_.getValuedStream.head).zipWithIndex.minBy(_._1._2)._2
//
//  // NOTE: not working?!
////  initStreams.map(_.getValuedStream.head).zipWithIndex.minBy(_._1._2) match {
////    case (mminHead, mminInd) =>
////      minHead = mminHead
////      minInd = mminInd
////    case _ =>
////  }
//  
//  // NOTE: not working?!
////  var minInd: Int = 0
////  var minHead = initStreams.head.getValuedStream.head
////
////  for (ind <- 1 until initStreams.size) {
////    if (initStreams(ind).getValuedStream.head._2 < minHead._2) {
////      minInd = ind
////      minHead = initStreams(ind).getValuedStream.head
////    }    
////  }
//  
//  override def getValuedStream = {
//    entering("getValuedStream")
//
//    val (minHead, minInd) = initStreams.map(_.getValuedStream.head).zipWithIndex.minBy(_._1._2)
//
//	  // set of iterators checked for next value
//	  def allIterators = Array(
//	    initStreams.map(_.getValuedStream.iterator.buffered) ++
//	    addedStreams.map( _.getValuedStream.iterator.buffered ) ++
//	    addedFilterables.map( new RecursiveStreamableIterator(_) )
//	  : _*)
//	  
//    minHead #:: {
//      // after the second element is enumerated, streams should not be added because they can change the stream
//      initialized = true
//      getNext(minInd, allIterators)
//    }
//  }
//
//  private def getNext(lastIndex: Int, allIterators: Array[BufferedIterator[IntegerWeightPair[T]]]):
//    Stream[IntegerWeightPair[T]] = {
//    entering("getNext", lastIndex)
//    
//    allIterators(lastIndex).next
//        
//    // at the end -1 means no next element was found
//    var minInd = -1
//    var minValue = Int.MaxValue
////    var minStream = Stream[IntegerWeightPair[T]]()
//    
//    fine("allIterators: " + allIterators.filter(_.hasNext).zipWithIndex.mkString(", "))
//    // check all iterators by going from first next after previously forwarded
//    for (
//      ind <- 1 to allIterators.size;
//      indToCheck = (lastIndex + ind) % allIterators.size;
//      iterator = allIterators(indToCheck);
//      if iterator.hasNext
//    ) {
//
//      finest("Checking iterator index %d".format(indToCheck))
//      if ( iterator.head._2 < minValue ) {
//        finest("Iterator ind %d, with head %d is smaller than minValue %d.".format(indToCheck, iterator.head._2, minValue))
//        minInd = indToCheck
//        minValue = iterator.head._2
////        minStream = iterator.head #:: {
////          getNext(minInd, allIterators)
////        }
//      }
//      
//    }
//
////    exiting("getMinIterator", minStream)
//    if (minInd >= 0) 
//      allIterators(minInd).head #:: {
//        getNext(minInd, allIterators)
//      }
//    else Stream.empty
//  }
//    
//  private var initialized = false
//      
//  private var addedFilterables = mutable.MutableList[OrderedCountable[T]]()
//  def getAddedFilterables = addedFilterables.toList
//  
//  private var addedStreams = mutable.MutableList[IntegerWeightStreamable[T]]()
//  def getAddedStreams = addedStreams.toList
//  
//  override def getStreamables = (initStreams ++ addedStreams ++ addedFilterables).toList
//    
//  // XXX terrible hack, since adding non-ordered streamable will break the code
//  override def addStreamable[U >: T](s: Streamable[U]) =
//    if (initialized) throw new UnsupportedOperationException("Cannot add new streamables once initialized")
//    else addedStreams += (s.asInstanceOf[IntegerWeightStreamable[T]])
//  
//  override def addStreamable[U >: T](s: Traversable[Streamable[U]]) =
//    if (initialized) throw new UnsupportedOperationException("Cannot add new streamables once initialized")
//    else addedStreams ++= (s.asInstanceOf[Traversable[IntegerWeightStreamable[T]]])
//  
//  override def addFilterable[U >: T](s: Counted[U]) =
//    if (initialized) throw new UnsupportedOperationException("Cannot add new streamables once initialized")
//    else addedFilterables += (s.asInstanceOf[OrderedCountable[T]])
//  
//  override def addFilterable[U >: T](s: Traversable[Counted[U]]) =
//    if (initialized) throw new UnsupportedOperationException("Cannot add new streamables once initialized")
//    else addedFilterables ++= (s.asInstanceOf[Traversable[OrderedCountable[T]]])
//  
//  override def isInitialized = initialized
//  
//  def initialize = { }
//
//  override def size = 
//    if (getStreamables.size == initStreams.size) initStreams.map(_.size).sum
//    else -1
//  
//  class RecursiveStreamableIterator(streamable: OrderedCountable[T]) extends BufferedIterator[IntegerWeightPair[T]]{
//
//    var nextToEnumerate = 0
//    
//    lazy val iterator = {
//      streamable.getValuedStream.iterator.buffered
//    }
//    
//    override def hasNext = {
//      info("hasNext: nextToEnumerate:%d < streamable.enumerated:%d".format(nextToEnumerate, streamable.enumerated))
//      nextToEnumerate < streamable.enumerated
//    }
//    
//    override def head = {
//      info("head: iterator.head" + iterator.head)
//      iterator.head
//    }
//    
//    override def next = {
//      info("next: nextToEnumerate=%d".format(nextToEnumerate))      
//      nextToEnumerate += 1
//      iterator.next
//    }
//    
//  }
//
//}
//
//object LazyRoundRobbin {
//	def apply[T](initStreams: Seq[IntegerWeightStreamable[T]]) =
//	  new LazyRoundRobbin(initStreams)
//	
//	def memoized[T](initStreams: Seq[IntegerWeightStreamable[T]]) =
//    new LazyRoundRobbin(initStreams) with Memoized[T]
//	
//	def counted[T](initStreams: Seq[IntegerWeightStreamable[T]]) =
//    new LazyRoundRobbin(initStreams) with OrderedCounted[T]
//}
