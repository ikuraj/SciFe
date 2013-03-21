package insynth.streams.ordered

import insynth.streams._
import insynth.streams.unordered.{ Singleton => UnordSingleton }

class Singleton[T](element: T) extends OrderedStreamable[T] {
  
  override def isInfinite = false
  
  override def isDepleted: Boolean = true // this does not matter
  override def nextReady(ind: Int): Boolean = ind == 0
    
  override def getStream = Stream(element)
  
  override def toString = element.toString
  
  override def getValues = Stream(1)
  
}

object Singleton {
  def apply[T](element: T) = new Singleton(element)
}