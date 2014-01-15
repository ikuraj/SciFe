package insynth.streams.ordered

import insynth.streams._
import insynth.streams.unordered.{ Singleton => UnordSingleton }

class Singleton[T](pair: (T, Int)) extends IntegerWeightStreamable[T] {
  
  def this(element: T, value: Int = 1) = this((element, value))
  
  override def size = 1
  
  override def toString = "ord.ST[ " + pair.toString + ']'
  
  override def getValuedStream = Stream( pair )
  
}

object Singleton {

  def apply[T](element: T) = new Singleton(element)

  def apply[T](pair: (T, Int)) = new Singleton(pair)

  def apply[T](element: T, value: Int) = new Singleton(element, value)

}