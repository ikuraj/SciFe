package insynth.streams
package unordered

import scala.collection.mutable.{ Map => MutableMap, LinkedList => MutableList, Set => MutableSet }

import insynth.streams.{ Streamable, AddStreamable }

/**
 * encapsulates round robbin and initialized when all streams are gathered
 * @param <T>
 * @param streams stream that form a new stream
 */
class LazyRoundRobbin[T](val initStreams: List[Streamable[T]]) extends Streamable[T] with AddStreamable[T] {
  
  override def size = -1
  
  var initialized = false
  
  var streams: List[Streamable[T]] = List.empty
    
  var innerRoundRobbin: RoundRobbin[T] = _
  
  // XXX terrible hack, fix this
  override def addStreamable[U >: T](s: Streamable[U]) =
    streams :+= (s.asInstanceOf[Streamable[T]])
  
  // XXX terrible hack, fix this
  override def addStreamable[U >: T](s: Traversable[Streamable[U]]) =
    streams ++= (s.asInstanceOf[Traversable[Streamable[T]]])
  
  override def addFilterable[U >: T](s: Counted[U]) =
    throw new RuntimeException
  
  override def addFilterable[U >: T](s: Traversable[Counted[U]]) =
    throw new RuntimeException
  
  override def isInitialized = initialized
  
  override def getStreamables = streams
  
  def initialize = {
    innerRoundRobbin = new RoundRobbin[T]((initStreams ++ streams).toSeq)
    initialized = true
  }
  
  override def isInfinite = 
    if (initialized) initStreams.exists( _.isInfinite )//innerRoundRobbin.isInfinite
    else false
    
  override def getStream =     
    if (initialized) innerRoundRobbin.getStream
    else Stream.Empty

}

object LazyRoundRobbin {
	def apply[T](initStreams: List[Streamable[T]]) = new LazyRoundRobbin(initStreams)
}