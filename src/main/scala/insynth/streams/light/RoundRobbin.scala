package insynth.streams
package light

import scala.collection.mutable

import insynth.util.logging._

class RoundRobbinMixed[T] protected[streams]
	(finiteStreams: Array[Enumerable[T]], infiniteStreams: Seq[Enumerable[T]])
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