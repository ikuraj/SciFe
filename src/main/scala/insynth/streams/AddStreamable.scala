package insynth.streams

import insynth.util.logging.HasLogger

trait AddStreamable[+T] {
  def addStreamable[U >: T](s: Streamable[U])
  
  def addStreamable[U >: T](s: Iterable[Streamable[U]])
  
  def isInitialized: Boolean
  
  def initialize: Unit
  
  def getStreams: List[Streamable[T]]
}