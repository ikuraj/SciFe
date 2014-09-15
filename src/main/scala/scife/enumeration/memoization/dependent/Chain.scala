package scife.enumeration
package memoization

import scife.enumeration.{ combinators => ecomb }
import scife.enumeration.dependent._

import scife.util.logging._

import scala.language.existentials

object Chain {
  
  def apply[I, O](s1: Enum[I], s2: Depend[I, O]) 
  	(implicit ms: MemoizationScope = null): Finite[(I, O)] = {
	    (s1, s2) match {
        case (Empty, _) => Empty
	      case (s: Singleton[I], df: DependFinite[_, _]) =>
	        df.apply(s.el) map { (o: O) => (s.el, o) }
	      case (f: Finite[I], df: DependFinite[I, O]) =>
	        this ! new ChainFinite(f, df) with Memoized[(I, O)]
	      case _ => throw new RuntimeException
	    }
  }
  
  def apply[I, O, R](s1: Enum[I], s2: Depend[I, O], combine: (I, O) => R) 
  	(implicit ms: MemoizationScope = null) = {
	    (s1, s2) match {
        case (Empty, _) => Empty
	      case (s: Singleton[I], df: DependFinite[_, _]) =>
	        //this !
	        this ! Map.memoized( df.apply(s.el), { (v: O) => combine(s.el, v) } )
	      case (f: Finite[I], df: DependFinite[_, _]) =>
	        this ! new ChainFiniteCombine(f, df, combine) with Memoized[R]
	    }
  }
  
  def apply[I, I2, O](s1: Enum[I], s2: Depend[I2, O], chain: I => I2)
  	(implicit ms: MemoizationScope = null) = {
	    (s1, s2) match {
      case (Empty, _) => Empty
      case (s: Singleton[I], df: DependFinite[I2, O]) =>
        this ! Map.memoized( df.apply( chain(s.el) ), { (v: O) => (s.el, v) } )
	      case (f: Finite[I], df: DependFinite[I2, O]) =>
	        this ! new ChainFiniteChain(f, df)( chain ) with Memoized[(I, O)]
	      case _ => throw new RuntimeException
	    }
  }
  
  def apply[I, I2, O, R](s1: Enum[I], s2: Depend[I2, O],
    chain: I => I2, combine: (I, O) => R) 
  	(implicit ms: MemoizationScope = null) = {
	    (s1, s2) match {
      case (Empty, _) => Empty
	//      case (f: Finite[I], df: DependFinite[_, _]) =>
	//        new ChainFiniteCombine(f, df, combine) with Memoized[R]
	      case _ => throw new RuntimeException
	    }
  }
  
	def ![T](m: Finite[T] with Memoized[T])(implicit ms: MemoizationScope) = {
	  if (ms != null) ms add m
	  m
	}
  
  def fin[I, O](s1: Finite[I], s2: DependFinite[I, O]) 
    (implicit ms: MemoizationScope = null) = {
      (s1, s2) match {
        case (Empty, _) => Empty
        case (s: Singleton[I], df: DependFinite[_, _]) =>
          df.apply(s.el) map { (o: O) => (s.el, o) }
        case (f: Finite[I], df: DependFinite[I, O]) =>
          this ! new ChainFinite(f, df) with Memoized[(I, O)]
        case _ => throw new RuntimeException
      }
  }
  
  def fin[I, O, R](s1: Finite[I], s2: DependFinite[I, O], combine: (I, O) => R) 
    (implicit ms: MemoizationScope = null) = {
      (s1, s2) match {
        case (Empty, _) => Empty
        case (s: Singleton[I], df: DependFinite[_, _]) =>
          //this !
          this ! Map.memoized( df.apply(s.el), { (v: O) => combine(s.el, v) } )
        case (f: Finite[I], df: DependFinite[_, _]) =>
          this ! new ChainFiniteCombine(f, df, combine) with Memoized[R]
      }
  }
  
//  def eager[I, O, R](s1: Enum[I], s2: Depend[I, O], combine: (I, O) => R) 
//  	(implicit ms: MemoizationScope = null) = {
//    val enum =
//	    (s1, s2) match {
//	      case (f: Finite[I], df: DependFinite[_, _]) =>
//	        new ChainFiniteCombine(f, df, combine) with Memoized[R]
//	    }
//    
//    if (ms != null) ms add enum
//    enum
//  }
  
}