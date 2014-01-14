package insynth.streams.ordered

import insynth.streams._

import insynth.util.logging.HasLogger

class LazyRoundRobbin[T](val initStreamsIn: IndexedSeq[OrderedStreamable[T]])
	extends OrderedStreamable[T] with AddStreamable[T] with HasLogger {

  override def isInfinite: Boolean = 
    if (initialized) innerRoundRobbin.isInfinite
    else false
    
  override def isDepleted: Boolean = 
    if (initialized) innerRoundRobbin.isDepleted
    else false
    
  override def nextReady(ind: Int): Boolean = { 
    if (ind == 1) {
      // TODO force stream evaluation, fix this
      val stream = produceRoundRobbin.getStream
    }
    if (initialized) innerRoundRobbin.nextReady(ind)
    else false
  }
    
  private def getMinIndex = {
    val valueIterators = initStreamsIn map { _.getValues.iterator.buffered }
    
    var min = Int.MaxValue
    var minInd = -1
    var ind = 0
    while (ind < valueIterators.size) {
      val indToCheck = ind % valueIterators.size
      
      if (valueIterators(indToCheck).hasNext && valueIterators(indToCheck).head < min) {
        min = valueIterators(indToCheck).head
        minInd = indToCheck
      }        
        
      ind += 1
    }
    
    assert(minInd > -1, "minInd > -1")
    info("returning from getMinIndex with minInd=" + minInd)
    (min, minInd)
  }
  
  lazy val (minValue, minInd) = getMinIndex
  
//  def mappedInitStreams = initStreamsIn.zipWithIndex map {
//    p =>
//      if (false && p._2 == minInd) {
////        if (p._1.isInfinite) SingleStream((p._1.getStream zip p._1.getValues).tail, p._1.isInfinite)
////        else FiniteStream((p._1.getStream zip p._1.getValues).tail)
//        SingleStream((p._1.getStream zip p._1.getValues).tail)
//      }
//      else p._1
//  }
  
  var initialized = false
      
  var streams = initStreamsIn
  
  override def getStreams = streams.toList
    
  var innerRoundRobbin: RoundRobbin[T] = _
  
  // XXX terrible hack, since adding non-ordered streamable will break the code
  override def addStreamable[U >: T](s: Streamable[U]) =
    streams :+= (s.asInstanceOf[OrderedStreamable[T]])
  
  override def addStreamable[U >: T](s: Iterable[Streamable[U]]) =
    streams ++= (s.asInstanceOf[Iterable[OrderedStreamable[T]]])
  
  override def isInitialized = initialized
    
  private def produceRoundRobbin = {
    if (innerRoundRobbin == null)
    	innerRoundRobbin = RoundRobbin[T](streams)
  	innerRoundRobbin
  } 
  
  override def initialize = {    
    produceRoundRobbin
    initialized = true
  }
    
  lazy val stream = 
    if (initialized && minInd > -1) initStreamsIn(minInd).getStream.head #:: produceRoundRobbin.getStream.tail
    else Stream.empty    
  
  override def getStream = {
    entering("getStream")
    info("initialized " + initialized)
    
    stream
  }
  
  override def getValues = 
    if (initialized && minInd > -1) {
      assert(minInd > -1)
      minValue #:: produceRoundRobbin.getValues.tail
    }
    else Stream.Empty
}

object LazyRoundRobbin {
	def apply[T](initStreams: IndexedSeq[OrderedStreamable[T]]) = new LazyRoundRobbin(initStreams)
}
