package insynth.streams.ordered

import insynth.streams._

import insynth.util.logging.HasLogger

class LazyRoundRobbin[T](val initStreamsIn: List[OrderedStreamable[T]])
	extends OrderedStreamable[T] with AddStreamable[T] with HasLogger {

  override def isInfinite: Boolean = 
    if (initialized) innerRoundRobbin.isInfinite
    else false
    
  override def isDepleted: Boolean = 
    if (initialized) innerRoundRobbin.isDepleted
    else false
    
  override def nextReady(ind: Int): Boolean = 
    if (initialized) innerRoundRobbin.nextReady(ind)
    else false
  
  var initialized = false
      
  var streams: List[OrderedStreamable[T]] = initStreamsIn
  
  override def getStreams = streams
    
  var innerRoundRobbin: RoundRobbin[T] = _
  
  // XXX terrible hack, since adding non-ordered streamable will break the code
  override def addStreamable[U >: T](s: Streamable[U]) =
    streams :+= (s.asInstanceOf[OrderedStreamable[T]])
  
  override def addStreamable[U >: T](s: Iterable[Streamable[U]]) =
    streams ++= (s.asInstanceOf[Iterable[OrderedStreamable[T]]])
  
  override def isInitialized = initialized
    
  private def produceRoundRobbin = {
    if (innerRoundRobbin == null)
    	innerRoundRobbin = RoundRobbin[T](getStreams)
  	innerRoundRobbin
  } 
  
  override def initialize = {    
    produceRoundRobbin
    initialized = true
  }
    
  lazy val stream = produceRoundRobbin.getStream
  
  override def getStream = stream
  
  override def getValues = produceRoundRobbin.getValues
}

object LazyRoundRobbin {
	def apply[T](initStreams: List[OrderedStreamable[T]]) = new LazyRoundRobbin(initStreams)
}
