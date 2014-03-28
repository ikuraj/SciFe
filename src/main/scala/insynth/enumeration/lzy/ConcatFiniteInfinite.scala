package insynth.enumeration
package lzy

import scala.reflect._
import scala.collection.mutable

import insynth.util.logging._

object ConcatFiniteInfinite {
  
  def apply[T](finites: Array[Finite[T]], infinites: Seq[Infinite[T]]) =
    (finites, infinites) match {
    	case (Array(fin), Seq(inf)) =>
    	  new ConcatFiniteInfiniteSingle(fin, inf)
    	case _ =>
    	  new ConcatFiniteInfiniteMul(finites, infinites)
  	}
  
  def apply[T](fin: Finite[T], inf: Infinite[T]) =
	  new ConcatFiniteInfiniteSingle(fin, inf)
  
}

trait ConcatFiniteInfinite[T] extends Infinite[T] with HasLogger {
  
  val finite: Finite[T]
  val infinite: Infinite[T]
  
  override def apply(ind: Int) = {
    if (ind < finite.size) finite(ind)
    else
      infinite(ind - finite.size)
  }
    
}

class ConcatFiniteInfiniteSingle[T] (
  override val finite: Finite[T], override val infinite: Infinite[T]
)	extends Infinite[T] with HasLogger {
    
}

class ConcatFiniteInfiniteMul[T] protected[enumeration]
	(finiteStreams: Array[Finite[T]], infiniteStreams: Seq[Infinite[T]])
  extends ConcatFiniteInfiniteSingle[T] with HasLogger {
  assert(finiteStreams.size > 0 && infiniteStreams.size > 0)
  
  override val finite = ConcatFinite.fixed(finiteStreams)
  override val infinite = ConcatInfinite(infiniteStreams)
  
}