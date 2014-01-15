package insynth.streams.ordered

import insynth.streams._
import insynth.streams.unordered.{ UnaryStream => UnUnaryStream }

/**
 * A Streamable that maps values of a given Streamable with the given function 
 * NOTE: modifyVal should be a monotonic function, otherwise your computer will blow up!
 * @param <T> @see Streamable
 * @param <U> @see OrderedStreamable
 */
object UnaryStream {
  def apply[T, U](streamable: IntegerWeightStreamable[T], modify: T=>U) =
    new UnaryStream(streamable, modify)
  
  def apply[T, U](streamable: IntegerWeightStreamable[T], modify: T=>U, modifyVal: Int => Int) =
    new UnaryStreamWithValueMod(streamable, modify, modifyVal)

  def memoized[T, U](streamable: IntegerWeightStreamable[T], modify: T=>U) =
    new UnaryStream(streamable, modify) with Memoized[U]
  
  def memoized[T, U](streamable: IntegerWeightStreamable[T], modify: T=>U, modifyVal: Int => Int) =
    new UnaryStreamWithValueMod(streamable, modify, modifyVal) with Memoized[U]

  def counted[T, U](streamable: IntegerWeightStreamable[T], modify: T=>U) =
    new UnaryStream(streamable, modify) with OrderedCountable[U]
  
  def counted[T, U](streamable: IntegerWeightStreamable[T], modify: T=>U, modifyVal: Int => Int) =
    new UnaryStreamWithValueMod(streamable, modify, modifyVal) with OrderedCountable[U]
}

class UnaryStream[T, U](val streamable: IntegerWeightStreamable[T], modify: T=>U)
	extends IntegerWeightStreamable[U] {
  
  override def size = streamable.size
  
  override def getValuedStream = streamable.getValuedStream map { p => (modify(p._1), p._2) }
  
}

class UnaryStreamWithValueMod[T, U](val streamable: IntegerWeightStreamable[T], modify: T=>U,
  modifyVal: Int => Int) extends IntegerWeightStreamable[U] {
  
  override def size = streamable.size
  
  override def getValuedStream = 
    streamable.getValuedStream map { p => (modify(p._1), modifyVal(p._2)) }
  
}