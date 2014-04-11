package insynth.enumeration
package dependent
package lzy

import insynth.{ enumeration => e }
import insynth.enumeration.lzy._

class ChainFinite[I, O]
  (override val left: Finite[I], override val right: DependFinite[I, O])
  extends combinators.Chain[I, O] with Finite[(I, O)] with HasLogger {
  
  val rr = ConcatFinite.buffer[(I, O)]( Seq.empty )
  
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
      rr.append( e.Product(Singleton(leftProduced), right(leftProduced)) )
    }
    rr(ind)
  }
  
}

class ChainFiniteCombine[I, O, R]
  (val left: Finite[I], val right: DependFinite[I, O], combine: (I, O) => R = (_: I, _: O))
  extends Finite[R] with HasLogger {
  
  val rr = ConcatFinite.buffer[R]( Seq.empty )
  
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
      val righEnum = right(leftProduced)
      val toAdd = e.Map(righEnum, { (rightProduced: O) => combine(leftProduced, rightProduced) })
      rr.append( toAdd )
    }
    rr(ind)
  }
  
}