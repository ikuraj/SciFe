package insynth.streams.ordered

import insynth.streams.OrderedStreamable
import insynth.streams.unordered.{ Empty => UnordEmpty }

object Empty extends OrderedStreamable[Nothing] {
    
  override def isInfinite = false

  override def isDepleted: Boolean = true
  override def nextReady(ind: Int): Boolean = false

  override def getStream = Stream.empty  
  
  override def getValues = Stream.empty
  
}