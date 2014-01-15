package insynth.streams.unordered

import insynth.streams.Streamable

/**
 * stream of single element
 * @param <T>
 * @param element
 */
class Singleton[T](element: T) extends Streamable[T] {

  override def size = 1

  override def isInfinite = false

  override def getStream = Stream(element)

}

object Singleton {
  def apply[T](element: T) = {
    new Singleton(element)
  }
}