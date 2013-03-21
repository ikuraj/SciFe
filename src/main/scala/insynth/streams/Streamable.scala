package insynth.streams

import insynth.util.logging.HasLogger

trait Streamable[+T] extends HasLogger {
  
  def isInfinite: Boolean
  def getStream: Stream[T]
  
}