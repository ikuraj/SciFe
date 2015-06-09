package scife.enumeration
package lzy

import combinators._

import scala.collection.mutable

import scife.util._

object ConcatInfinite {

  def apply[T](s1: Infinite[T], s2: Infinite[T]) =
    new ConcatInfinite( Array(s1, s2) )

  def apply[T](streams: Array[Infinite[T]]) =
    new ConcatInfinite(streams)

}

class ConcatInfinite[T] (val enumArray: Array[Infinite[T]])
  extends ConcatMul[T] with Infinite[T] with HasLogger {
//  assert(enums.forall(_.size == -1), "RoundRobbinInfinite should be constructed " +
//    "with infinite streams. " +
//    "(sizes are %s)".format(enums.map(_.size).distinct))
  
  override def enums = enumArray.toSeq

  override def apply(ind: Int) = {
    val arrInd = ind % enumArray.length
    val elInd = ind / enumArray.length
    enumArray(arrInd)(elInd)
  }

}
