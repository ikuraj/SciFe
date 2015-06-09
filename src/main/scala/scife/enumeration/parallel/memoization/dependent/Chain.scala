package scife.enumeration
package parallel
package memoization

import scife.enumeration.{ combinators => ecomb }
import scife.enumeration.dependent._
import scope._
import scife.enumeration.memoization._

import scife.util.logging._

import scala.language.existentials
import scala.reflect._

object Chain {
  
  import util._

//  def apply[I, O](s1: Enum[I], s2: Depend[I, O])(implicit ms: MemoizationScope, ct: ClassTag[(I, O)]): Finite[(I, O)] = {
//    (s1, s2) match {
//      case (Empty, _) => Empty
//      case (s: Singleton[I], df: DependFinite[_, _]) =>
//        df.apply(s.el) map { (o: O) => (s.el, o) }
//      case (f: Finite[I], df: DependFinite[I, O]) =>
//        ! new {
//          override val classTagT = ct
//        } with ChainFinite(f, df) with MemoizedSize with MemoizedStatic[(I, O)]
//      case _ => throw new RuntimeException
//    }
//  }
//
//  def apply[I, O, R](s1: Enum[I], s2: Depend[I, O], combine: (I, O) => R)
//    (implicit ms: MemoizationScope, ct: scala.reflect.ClassTag[R]) = {
//    (s1, s2) match {
//      case (Empty, _) => Empty
//      case (s: Singleton[I], df: DependFinite[_, _]) =>
//        ! Map.memoized(df.apply(s.el), { (v: O) => combine(s.el, v) })
//      case (f: Finite[I], df: DependFinite[_, _]) =>
//        ! new {
//          override val classTagT = ct
//        } with ChainFiniteCombine(f, df, combine) with MemoizedSize with MemoizedStatic[R]
//    }
//  }
  
  def breadthSearch[I, O, R](s1: Enum[I], s2: Depend[I, O], combine: (I, O) => R)
    (implicit ms: MemoizationScope, ct: scala.reflect.ClassTag[R]) = {
    (s1, s2) match {
      case (Empty, _) => Empty
      case (s: Singleton[I], df: DependFinite[_, _]) =>
        ! new {
          override val classTagT = ct
        } with Map(df.apply(s.el), { (v: O) => combine(s.el, v) }) with Finite[R] with MemoizedSize with MemoizedStaticOpt[R]
//        ! Map.memoized(df.apply(s.el), { (v: O) => combine(s.el, v) })
      case (f: Finite[I], df: DependFinite[_, _]) =>
        ! new {
          override val classTagT = ct
        } with breadth.ChainFiniteCombine(f, df, combine) with MemoizedSize with MemoizedStaticOpt[R]
    }
  }
  
  def breadthSearchPar[I, O, R](s1: Enum[I], s2: Depend[I, O], combine: (I, O) => R)
    (implicit ms: MemoizationScope, ct: scala.reflect.ClassTag[R]) = {
    import scife.enumeration.memoization.lzy
    
    (s1, s2) match {
      case (Empty, _) => Empty
      case (e, _) if e.size == 0 => Empty
//      case (e: ChainFiniteCombine[I], _) if 
      case (s: Singleton[I], df: DependFinite[_, _]) =>
        ! new {
          override val classTagT = ct
        } with Map(df.apply(s.el), { (v: O) => combine(s.el, v) }) with Finite[R] with lzy.MemoizedSize with lzy.MemoizedStaticOpt[R]
//        ! Map.memoized(df.apply(s.el), { (v: O) => combine(s.el, v) })
      case (f: Finite[I], df: DependFinite[_, _]) =>
        ! new {
          override val classTagT = ct
        } with parallel.breadth.ChainFiniteCombine(f, df, combine) with lzy.MemoizedSize with lzy.MemoizedStaticOpt[R]
    }
  }

//  def apply[I, I2, O](s1: Enum[I], s2: Depend[I2, O], chain: I => I2)(implicit ms: MemoizationScope, ct: ClassTag[(I, O)]) = {
//    (s1, s2) match {
//      case (Empty, _) => Empty
//      case (s: Singleton[I], df: DependFinite[I2, O]) =>
//        ! Map.memoized(df.apply(chain(s.el)), { (v: O) => (s.el, v) })
//      case (f: Finite[I], df: DependFinite[I2, O]) =>
//        ! new {
//          override val classTagT = ct
//        } with ChainFiniteChain(f, df)(chain) with MemoizedSize with MemoizedStatic[(I, O)]
//      case _ => throw new RuntimeException
//    }
//  }
//
//  def apply[I, I2, O, R](s1: Enum[I], s2: Depend[I2, O],
//    chain: I => I2, combine: (I, O) => R)(implicit ms: MemoizationScope) = {
//    (s1, s2) match {
//      case (Empty, _) => Empty
//      //      case (f: Finite[I], df: DependFinite[_, _]) =>
//      //        new ChainFiniteCombine(f, df, combine) with Memoized[R]
//      case _ => throw new RuntimeException
//    }
//  }
//
//  def fin[I, O](s1: Finite[I], s2: DependFinite[I, O])(implicit ms: MemoizationScope, ct: ClassTag[(I, O)]) = {
//    (s1, s2) match {
//      case (Empty, _) => Empty
//      case (s: Singleton[I], df: DependFinite[_, _]) =>
//        df.apply(s.el) map { (o: O) => (s.el, o) }
//      case (f: Finite[I], df: DependFinite[I, O]) =>
//        ! new {
//          override val classTagT = ct
//        } with ChainFinite(f, df) with MemoizedSize  with MemoizedStatic[(I, O)]
//      case _ => throw new RuntimeException
//    }
//  }
//
//  def dynamic[I, O, R](s1: Enum[I], s2: Depend[I, O], combine: (I, O) => R)
//    (implicit ms: MemoizationScope, ct: scala.reflect.ClassTag[R]) = {
//    (s1, s2) match {
//      case (Empty, _) => Empty
//      case (s: Singleton[I], df: DependFinite[_, _]) =>
//        ! Map.memoizedDyn(df.apply(s.el), { (v: O) => combine(s.el, v) })
//      case (f: Finite[I], df: DependFinite[_, _]) =>
//        ! new ChainFiniteCombine(f, df, combine) with MemoizedSize with MemoizedDynamic[R]
//    }
//  }
//  
//  def single[I, O](s1: Finite[I], s2: DependFinite[I, O])
//    (implicit ms: MemoizationScope, ct: scala.reflect.ClassTag[O]): Finite[O] = {
//    (s1, s2) match {
//      case (Empty, _) => Empty
//      case (s: Singleton[I], df: DependFinite[I, O]) => df.apply(s.el)
//      case (f: Finite[I], df: DependFinite[I, O]) =>
//        ! new ChainFiniteSingle(f, df) with MemoizedSize with MemoizedDynamic[O]
//    }
//  }

  //  def fin[I, O, R](s1: Finite[I], s2: DependFinite[I, O], combine: (I, O) => R)
  //    (implicit ms: MemoizationScope = NoScope) = {
  //      (s1, s2) match {
  //        case (Empty, _) => Empty
  //        case (s: Singleton[I], df: DependFinite[_, _]) =>
  //          ! Map.memoized( df.apply(s.el), { (v: O) => combine(s.el, v) } )
  //        case (f: Finite[I], df: DependFinite[_, _]) =>
  //          ! new ChainFiniteCombine(f, df, combine) with Memoized[R]
  //      }
  //  }
  //  def eager[I, O, R](s1: Enum[I], s2: Depend[I, O], combine: (I, O) => R)
  //    (implicit ms: MemoizationScope = null) = {
  //    val enum =
  //      (s1, s2) match {
  //        case (f: Finite[I], df: DependFinite[_, _]) =>
  //          new ChainFiniteCombine(f, df, combine) with Memoized[R]
  //      }
  //
  //    if (ms != null) ms add enum
  //    enum
  //  }

}
