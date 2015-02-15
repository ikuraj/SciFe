package scife.enumeration
package lzy

import combinators._

import scala.collection.mutable
import scala.reflect._

import scife.util._

object ConcatFiniteInfinite {

  def apply[T](finites: Array[Finite[T]], infinites: Array[Infinite[T]]) =
    (finites, infinites) match {
      case (Array(fin), Array(inf)) =>
        new ConcatFiniteInfiniteSingle(fin, inf)
      case _ =>
        new ConcatFiniteInfiniteMul(finites, infinites)
    }

  def apply[T](fin: Finite[T], inf: Infinite[T]) =
    new ConcatFiniteInfiniteSingle(fin, inf)

}

trait ConcatFiniteInfinite[T] extends Concat[T, T, T] with Infinite[T] with HasLogger {

  val finite: Finite[T]
  val infinite: Infinite[T]

  override val left = finite
  override val right = infinite

  override def apply(ind: Int) = {
    if (ind < finite.size) finite(ind)
    else
      infinite(ind - finite.size)
  }

}

class ConcatFiniteInfiniteSingle[T] (
  override val finite: Finite[T], override val infinite: Infinite[T]
)  extends ConcatFiniteInfinite[T] with HasLogger {

}

class ConcatFiniteInfiniteMul[T] protected[enumeration]
  (finiteStreams: Array[Finite[T]], infiniteStreams: Array[Infinite[T]])
  extends ConcatFiniteInfinite[T] with HasLogger {
  assert(finiteStreams.size > 0 && infiniteStreams.size > 0)

  override val finite = ConcatFinite.fixed(finiteStreams)
  override val infinite = ConcatInfinite(infiniteStreams)

}
