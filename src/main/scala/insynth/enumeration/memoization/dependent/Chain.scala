package insynth.enumeration
package memoization

import insynth.enumeration.{ combinators => ecomb }
import insynth.enumeration.dependent._

import insynth.util.logging._

import scala.language.existentials

object Chain {
  
  def apply[I, O](s1: Enum[I], s2: Depend[I, O]) 
  	(implicit ms: MemoizationScope = null) = {
    val enum =
	    (s1, s2) match {
	      case (f: Finite[I], df: DependFinite[I, O]) =>
	        new ChainFinite(f, df) with Memoized[(I, O)]
	      case _ => throw new RuntimeException
	    }
    
    if (ms != null) ms add enum
    enum
  }
  
  def apply[I, O, R](s1: Enum[I], s2: Depend[I, O], combine: (I, O) => R) 
  	(implicit ms: MemoizationScope = null) = {
    val enum =
	    (s1, s2) match {
	      case (f: Finite[I], df: DependFinite[_, _]) =>
	        new ChainFiniteCombine(f, df, combine) with Memoized[R]
	    }
    
    if (ms != null) ms add enum
    enum
  }
  
  def apply[I, I2, O](s1: Enum[I], s2: Depend[I2, O], chain: I => I2)
  	(implicit ms: MemoizationScope = null): Memoized[(I, O)] = {
    val enum =
	    (s1, s2) match {
	      case (f: Finite[I], df: DependFinite[I2, O]) =>
	        new ChainFiniteChain(f, df)( chain ) with Memoized[(I, O)]
	      case _ => throw new RuntimeException
	    }
    
    if (ms != null) ms add enum
    enum
  }
  
  def apply[I, I2, O, R](s1: Enum[I], s2: Depend[I2, O],
    chain: I => I2, combine: (I, O) => R) 
  	(implicit ms: MemoizationScope = null) = {
    val enum =
	    (s1, s2) match {
	//      case (f: Finite[I], df: DependFinite[_, _]) =>
	//        new ChainFiniteCombine(f, df, combine) with Memoized[R]
	      case _ => throw new RuntimeException
	    }
    
    if (ms != null) ms add enum
    enum
  }
  
}