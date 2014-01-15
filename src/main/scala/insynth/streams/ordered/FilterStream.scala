package insynth.streams.ordered

import insynth.streams._
import insynth.streams.unordered.{ UnaryStream => UnUnaryStream }

import scala.annotation.tailrec

class FilterStream[T](val streamable: IntegerWeightStreamable[T], filterFun: T => Boolean)
	extends IntegerWeightStreamable[T] with Filterable[T] {
  
  override def getValuedStream = streamable.getValuedStream filter { p => filterFun(p._1) }
  
//  lazy val st = streamable.getValuedStream filter { p => filterFun(p._1) }
//  
//  override def getValuedStream = st
  
  override def size = -1
  
}

class FilterStreamCounted[T](val streamable: IntegerWeightStreamable[T], filterFun: T => Boolean)
  extends IntegerWeightStreamable[T] with OrderedCounted[T] with Filterable[T] {
  
  var enumeratedCount = 0
  
  lazy val st = streamable.getValuedStream filter { p =>
    val res = filterFun(p._1)
    if (res) enumeratedCount+= 1
    res
  }
  
  override def enumerated = enumeratedCount
  
  override def getValuedStream = st
  
  override def size = -1
  
}

object FilterStream {
  def apply[T](streamable: IntegerWeightStreamable[T], filterFun: T => Boolean) =
    new FilterStream(streamable, filterFun)

  def memoized[T](streamable: IntegerWeightStreamable[T], filterFun: T => Boolean) =
    new FilterStream(streamable, filterFun) with Memoized[T]

  def counted[T](streamable: IntegerWeightStreamable[T], filterFun: T => Boolean) =
    new FilterStreamCounted(streamable, filterFun)
//    new FilterStream(streamable, filterFun) with OrderedCounted[T]
}