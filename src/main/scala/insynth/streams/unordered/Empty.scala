package insynth.streams.unordered

import insynth.streams.Streamable

object Empty extends Streamable[Nothing] {

  override def isInfinite = false

  override def getStream = Stream.empty  
  
  override def size = 0
}