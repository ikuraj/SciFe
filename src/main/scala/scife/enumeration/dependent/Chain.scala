package scife.enumeration
package dependent

import lzy._

import scife.util.logging._

object Chain {

  def apply[I, O](s1: Enum[I], s2: Depend[I, O]): Enum[(I, O)] = {
    (s1, s2) match {
      case (Empty, _) => Empty
      case (s: Singleton[I], df: DependFinite[I, O]) => Map(df.apply(s.el),
        (x: O) => { (s.el, x) })
      case (f: Finite[I], df: DependFinite[I, O]) =>
        new ChainFinite(f, df)
    }
  }

  def single[I, O](s1: Enum[I], s2: Depend[I, O]): Enum[O] = {
    (s1, s2) match {
      case (Empty, _) => Empty
      case (s: Singleton[I], df: DependFinite[I, O]) => df.apply(s.el)
      case (f: Finite[I], df: DependFinite[I, O]) =>
        new ChainFiniteSingle(f, df)
    }
  }
  
  def single[I, O](s1: Finite[I], s2: DependFinite[I, O]): Finite[O] = {
    (s1, s2) match {
      case (Empty, _) => Empty
      case (s: Singleton[I], df: DependFinite[I, O]) => df.apply(s.el)
      case (f: Finite[I], df: DependFinite[I, O]) =>
        new ChainFiniteSingle(f, df)
    }
  }

  def apply[I, O, R](s1: Enum[I], s2: Depend[I, O], combine: (I, O) => R) = {
    (s1, s2) match {
      case (Empty, _) => Empty
      case (s: Singleton[I], df: DependFinite[I, O]) =>
        Map( df.apply(s.el), { (v: O) => combine(s.el, v) } )
      case (f: Finite[I], df: DependFinite[I, O]) =>
        new ChainFiniteCombine(f, df, combine)
    }
  }

  def apply[I, I2, O](s1: Enum[I], s2: Depend[I2, O], chain: I => I2): Enum[(I, O)] = {
    (s1, s2) match {
      case (Empty, _) => Empty
      case (s: Singleton[I], df: DependFinite[I2, O]) =>
        Map( df.apply( chain(s.el) ), { (v: O) => (s.el, v) } )
      case (f: Finite[I], df: DependFinite[_, _]) =>
        new ChainFiniteChain(f, df)( chain )
    }
  }

  def single[I, I2, O](s1: Enum[I], s2: Depend[I2, O], chain: I => I2): Enum[O] = {
    (s1, s2) match {
      case (Empty, _) => Empty
      case (s: Singleton[I], df: DependFinite[I, O]) =>
        df.apply( chain(s.el) )
      case (f: Finite[I], df: DependFinite[I2, O]) =>
        new ChainFiniteChainSingle(f, df)( chain )
    }
  }

  def apply[I, I2, O, R](s1: Enum[I], s2: Depend[I2, O],
    chain: I => I2, combine: (I, O) => R) = {
    (s1, s2) match {
      case (Empty, _) => Empty
      case (s: Singleton[I], df: DependFinite[_, _]) =>
       throw new RuntimeException
//      case (f: Finite[I], df: DependFinite[_, _]) =>
//        new ChainFiniteCombine(f, df, combine)
      case _ => throw new RuntimeException
    }
  }

  def apply[I, O](s1: Finite[I], s2: DependFinite[I, O]): Finite[(I, O)] = {
    (s1, s2) match {
      case (Empty, _) => Empty
      case (s: Singleton[I], df: DependFinite[I, O]) => Map(df.apply(s.el), { (s.el, (_: O)) } )
      case (f: Finite[I], df: DependFinite[I, O]) =>
        new ChainFinite(f, df)
    }
  }

  def apply[I, O, R](s1: Finite[I], s2: DependFinite[I, O], combine: (I, O) => R): Finite[R] = {
    (s1, s2) match {
      case (Empty, _) => Empty
      case (s: Singleton[I], df: DependFinite[I, O]) =>
        Map( df.apply(s.el), { (v: O) => combine(s.el, v) } )
      case (f: Finite[I], df: DependFinite[I, O]) =>
        new ChainFiniteCombine(f, df, combine)
    }
  }

  def apply[I, I2, O](s1: Finite[I], s2: DependFinite[I2, O], chain: I => I2): Finite[(I, O)] = {
    (s1, s2) match {
      case (Empty, _) => Empty
      case (s: Singleton[I], df: DependFinite[I2, O]) =>
        Map( df.apply( chain(s.el) ), { (v: O) => (s.el, v) } )
      case (f: Finite[I], df: DependFinite[_, _]) =>
        new ChainFiniteChain(f, df)( chain )
    }
  }

  def single[I, I2, O](s1: Finite[I], s2: DependFinite[I2, O], chain: I => I2): Finite[O] = {
    (s1, s2) match {
      case (Empty, _) => Empty
      case (s: Singleton[I], df: DependFinite[I, O]) =>
        df.apply( chain(s.el) )
      case (f: Finite[I], df: DependFinite[I2, O]) =>
        new ChainFiniteChainSingle(f, df)( chain )
    }
  }

  def apply[I, I2, O, R](s1: Finite[I], s2: DependFinite[I2, O],
    chain: I => I2, combine: (I, O) => R) = {
    (s1, s2) match {
      case (Empty, _) => Empty
      case (s: Singleton[I], df: DependFinite[_, _]) =>
       throw new RuntimeException
//      case (f: Finite[I], df: DependFinite[_, _]) =>
//        new ChainFiniteCombine(f, df, combine)
      case _ => throw new RuntimeException
    }
  }

}
