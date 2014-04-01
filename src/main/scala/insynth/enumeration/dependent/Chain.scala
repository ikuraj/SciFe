package insynth.enumeration
package dependent

import lzy._

import insynth.util.logging._

object Chain {
  
  def apply[I, O](s1: Enum[I], s2: Depend[I, O]) = {
    (s1, s2) match {
      case (f: Finite[I], df: DependFinite[_, _]) =>
        new ChainFinite(f, df)
      case _ => throw new RuntimeException
    }    
  }
  
  def apply[I, O, R](s1: Enum[I], s2: Depend[I, O], combine: (I, O) => R) = {
    (s1, s2) match {
      case (f: Finite[I], df: DependFinite[_, _]) =>
        new ChainFiniteCombine(f, df, combine)
      case _ => throw new RuntimeException
    }    
  }
  
  def apply[I, I2, O](s1: Enum[I], s2: Depend[I2, O], chain: I => I2): Enum[(I, O)] = {
    (s1, s2) match {
      case (f: Finite[I], df: DependFinite[_, _]) =>
        new ChainFiniteChain(f, df)( chain )
      case _ => throw new RuntimeException
    }
  }
  
  def single[I, I2, O](s1: Enum[I], s2: Depend[I2, O], chain: I => I2): Enum[O] = {
    (s1, s2) match {
      case (f: Finite[I], df: DependFinite[I2, O]) =>
        new ChainFiniteChainSingle(f, df)( chain )
      case _ => throw new RuntimeException
    }    
  }
  
  def apply[I, I2, O, R](s1: Enum[I], s2: Depend[I2, O],
    chain: I => I2, combine: (I, O) => R) = {
    (s1, s2) match {
//      case (f: Finite[I], df: DependFinite[_, _]) =>
//        new ChainFiniteCombine(f, df, combine)
      case _ => throw new RuntimeException
    }    
  }
  
}