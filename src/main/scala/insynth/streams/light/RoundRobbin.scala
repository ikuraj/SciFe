package insynth.streams
package light

import scala.reflect._
import scala.collection.mutable

import insynth.util.logging._

object RoundRobbin {
  
  def apply[@specialized T](left: Enum[T], right: Enum[T])(implicit ct: ClassTag[T]) =
    (left, right) match {
      case (left: Finite[T], right: Infinite[T]) =>        
        new RoundRobbinMixedSingle(left, right)
      case _ =>
        new RoundRobbinFiniteFixed(Array(left, right))
    }
}

class RoundRobbinMixed[T] protected[streams]
	(finiteStreams: Array[Enum[T]], infiniteStreams: Seq[Enum[T]])
  extends Infinite[T] with HasLogger {
  assert(finiteStreams.size > 0 && infiniteStreams.size > 0)
  
  val finite = RoundRobbinFinite.fixed(finiteStreams)
  val infinite = RoundRobbinInfinite(infiniteStreams)
  
  override def apply(ind: Int) = {
    if (ind < finite.size) finite(ind)
    else
      infinite(ind - finite.size)
  }
    
}

class RoundRobbinMixedSingle[T] protected[streams]
  (finite: Finite[T], infinite: Infinite[T])
  extends Infinite[T] with HasLogger {
  
  override def apply(ind: Int) = {
    if (ind < finite.size) finite(ind)
    else
      infinite(ind - finite.size)
  }
    
}