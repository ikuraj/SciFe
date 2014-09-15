package scife.enumeration
package dependent

import combinators.ChainSingle
import scife.{ enumeration => e }

class ChainFiniteSingle[I, O]
  (override val left: Finite[I], override val right: DependFinite[I, O])
  extends ChainSingle[I, O] with Finite[O] with HasLogger {
  
  val rr = { 
    val rightStreams = 
      for (ind <- 0 until left.size; stream = right.getEnum(left(ind)); if stream.size > 0 )
        yield stream
        
    e.lzy.ConcatFinite.fixed[O](
      Array(rightStreams: _*)
    )
  }
  
  override def size = rr.size
  
  override def apply(ind: Int) =
    rr(ind)
  
}

class ChainFiniteChainSingle[I, I2, O]
  (s1: Finite[I], s2: DependFinite[I2, O])(chain: I => I2)
  extends Finite[O] with HasLogger {
  
  val rr = { 
    val rightStreams = 
      for (ind <- 0 until s1.size; stream = s2.getEnum( chain(s1(ind)) ); if stream.size > 0 )
        yield stream
        
    e.lzy.ConcatFinite.fixed[O](
      Array(rightStreams: _*)
    )
  }
  
  override def size = rr.size
  
  override def apply(ind: Int) =
    rr(ind)
  
}