package insynth.streams.ordered

import insynth.streams.OrderedStreamable
import insynth.streams.unordered.{ Empty => UnordEmpty }

object Empty extends IntegerWeightStreamable[Nothing] {
    
  override def size = 0

  override def getValuedStream = Stream.empty
  
}