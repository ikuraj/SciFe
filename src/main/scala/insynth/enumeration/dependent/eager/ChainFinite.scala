package insynth.enumeration
package dependent

import combinators._
import insynth.{ enumeration => e }

class ChainFinite[I, O]
  (override val left: Finite[I], override val right: DependFinite[I, O])
  extends Chain[I, O] with Finite[(I, O)] with HasLogger {
  
  val rr = {
    val rightStreams = 
      for (
        ind <- 0 until left.size; leftEl = left(ind);
        stream = right.getEnum(leftEl); if stream.size > 0
      )
        yield e.Product(Singleton(leftEl), stream)
        
    e.lzy.ConcatFinite.fixed(
      Array(rightStreams: _*)
    )
  }
  
  override def size = rr.size
  
  override def apply(ind: Int) =
    rr(ind)
  
}

// optimization classes that incorporate chain and combine

class ChainFiniteChain[I, I2, O]
  (val left: Finite[I], val right: DependFinite[I2, O])(chain: I => I2)
  extends Finite[(I, O)] with HasLogger {
  
  val rr = {
    val rightStreams = 
      for (
        ind <- 0 until left.size; leftEl = left(ind);
        stream = right.getEnum( chain(leftEl) ); if stream.size > 0
      )
        yield e.Product(Singleton(leftEl), stream)
        
    e.lzy.ConcatFinite.fixed(
      Array(rightStreams: _*)
    )
  }
  
  override def size = rr.size
  
  override def apply(ind: Int) =
    rr(ind)
  
}

class ChainFiniteCombine[I, O, R]
  (s1: Finite[I], s2: DependFinite[I, O], combine: (I, O) => R)
  extends Finite[R] with HasLogger {
  
  val rr = {
    val streams = 
      for (ind <- 0 until s1.size; leftProduced = s1(ind); rightStream = s2.getEnum( leftProduced );
        if rightStream.size > 0 ) yield {
          e.Map( rightStream, { (rightProduced: O) => combine(leftProduced, rightProduced) })
        }

    e.lzy.ConcatFinite.fixed[R](  
      Array(streams: _*)
    )
  }
  
  override def size = rr.size
  
  override def apply(ind: Int) =
    rr(ind)
  
}

class ChainFiniteChainCombine[I, I2, O, R]
  (s1: Finite[I], s2: DependFinite[I2, O], chain: I => I2, combine: (I, O) => R)
  extends Finite[R] with HasLogger {
  
  val rr = {
    val streams = 
      for (ind <- 0 until s1.size; leftProduced = s1(ind); rightStream = s2.getEnum( chain(leftProduced) );
        if rightStream.size > 0 ) yield {
          e.Map( rightStream, { (rightProduced: O) => combine(leftProduced, rightProduced) })
        }

    e.lzy.ConcatFinite.fixed[R](  
      Array(streams: _*)
    )
  }
  
  override def size = rr.size
  
  override def apply(ind: Int) =
    rr(ind)
  
}