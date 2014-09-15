package scife.enumeration
package dependent
package lzy

import scife.{ enumeration => e }
import scife.enumeration.lzy._

class ChainFiniteSingle[I, O]
  (override val left: Finite[I], override val right: DependFinite[I, O])
  extends combinators.ChainSingle[I, O] with Finite[O] with HasLogger {
  
  val rr = ConcatFinite.buffer[O]( Seq.empty )
  
  override def size: Int = {
    var size = 0
    for( ind <- 0 until left.size; innerEnum = right( left(ind) ) ) yield
      if ( innerEnum.size == -1 ) return -1
      else size += innerEnum.size
    size
  }
  
  var explored = -1
  
  override def apply(ind: Int) = {
    while(ind >= rr.size) {
      explored += 1
      if (explored >= left.size) throw new NoSuchElementException("Went out of range of this lazy structure")
      val leftProduced = left(explored)
      rr.append( right(leftProduced) )
    }
    rr(ind)
  }
  
}