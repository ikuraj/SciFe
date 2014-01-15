package insynth.streams

import insynth.util.logging.HasLogger

/**
 * AddStreamable is a Stremable to which other Streamables may be added before the internal
 * mechanism on how to constraint those Stremables is established - this operation can be
 * achieved with initialize
 * @param <T> Type of enumerated elements
 */
trait AddStreamable[+T] {
  def addStreamable[U >: T](s: Streamable[U])
  
  def addStreamable[U >: T](s: Traversable[Streamable[U]])
  
  def addFilterable[U >: T](s: Counted[U])
  
  def addFilterable[U >: T](s: Traversable[Counted[U]])
  
  // NOTE: the streamable is initialized once the getStream method is called
  def isInitialized: Boolean
  
  def initialize: Unit
  
  def getStreamables: List[Streamable[T]]
}