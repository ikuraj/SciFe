package insynth.streams.ordered

import insynth.streams._
import insynth.streams.unordered.{ SingleStream => UnSingleStream }

/**
 * Wrapper around the Scala stream
 * NOTE: parameter stream needs to be ordered itself
 */
class WrapperStream[T](stream: Stream[(T, Int)])
	extends IntegerWeightStreamable[T] {
  
  override def isInfinite = true
  
  override def getValuedStream = stream
    
  override def size = -1
}

object WrapperStream {
  def apply[T](stream: Seq[(T, Int)]) =
    if (stream.hasDefiniteSize)
      FiniteStream(stream)
    else
      new WrapperStream(stream.toStream)

  def apply[T](stream: Stream[(T, Int)], isInfinite: Boolean) =
    if (isInfinite)
      new WrapperStream(stream.toStream)
    else
      FiniteStream(stream.toList)

  def apply(el: Int) =
    new Singleton(el, el)

  def counted[T](stream: Seq[(T, Int)]) =
    if (stream.hasDefiniteSize)
      FiniteStream.counted(stream)
    else
      new WrapperStream(stream.toStream) with OrderedCountable[T]

  def counted[T](stream: Stream[(T, Int)], isInfinite: Boolean) =
    if (isInfinite)
      new WrapperStream(stream.toStream) with OrderedCountable[T]
    else
      FiniteStream.counted(stream.toList)

  def counted(el: Int) =
    new Singleton(el, el) with OrderedCountable[Int]
}