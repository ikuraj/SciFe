package insynth.enumeration
package lzy

import combinators._

import insynth.util.logging._

import scala.collection.mutable

object ConcatInfinite {

  def apply[T](streams: Seq[Infinite[T]]) =
    new ConcatInfinite(streams)

}

class ConcatInfinite[T] (override val enums: Seq[Infinite[T]])
  extends ConcatMul[T, T, T] with Infinite[T] with HasLogger {
  assert(enums.forall(_.size == -1), "RoundRobbinInfinite should be constructed " +
		"with infinite streams. " +
    "(sizes are %s)".format(enums.map(_.size).distinct))
  
  val enumArray = enums.toArray
  
  override def apply(ind: Int) = {
    val arrInd = ind % enumArray.size
    val elInd = ind / enumArray.size
    enumArray(arrInd)(elInd)
  }
    
}