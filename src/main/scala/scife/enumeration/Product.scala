package scife.enumeration

import lzy._

import scala.language.higherKinds
import scala.reflect._

object Product {

  def apply[T, V](s1: Enum[T], s2: Enum[V]): Enum[(T, V)] =
    (s1, s2) match {
      case (Empty, _) | (_, Empty) =>
        Empty
      case (s1: Singleton[T], s2: Finite[V]) =>
        new ProductSingleton(s1, s2)
      case (s1: Finite[T], s2: Singleton[V]) =>
        new ProductSingletonRight(s1, s2)
      case (s1: Finite[T], s2: Finite[V]) => new ProductFinite(s1, s2)
      case (s1: Infinite[T], s2: Infinite[V]) => new ProductInfinite(s1, s2)
      case (s1: Finite[T], s2: Infinite[V]) => new ProductFiniteInfiniteLeft(s1, s2)
      case (s1: Infinite[T], s2: Finite[V]) => new ProductFiniteInfiniteRight(s1, s2)
    }

  def apply[T, V, U](s1: Enum[T], s2: Enum[V], combine: (T, V) => U): Enum[U] =
    (s1, s2) match {
      case (Empty, _) | (_, Empty) =>
        Empty
      case (s1: Singleton[T], s2: Finite[V]) =>
        Map(s2, { (v: V) => combine(s1.el, v) })
      case (s1: Finite[T], s2: Singleton[V]) =>
        Map(s1, { (v: T) => combine(v, s2.el) })
      case (s1: Finite[T], s2: Finite[V]) => new ProductFiniteComb(s1, s2)(combine)
      case (s1: Infinite[T], s2: Infinite[V]) => new ProductInfiniteComb(s1, s2)(combine)
      case (s1: Finite[T], s2: Infinite[V]) => new ProductFiniteInfiniteComb(s1, s2)(combine)
    }

  def apply[T, V](s1: Finite[T], s2: Finite[V]): Finite[(T, V)] =
    (s1, s2) match {
      case (Empty, _) | (_, Empty) =>
        Empty
      case (s1: Singleton[T], s2: Finite[V]) =>
        new ProductSingleton(s1, s2)
      case (s1: Finite[T], s2: Singleton[V]) =>
        new ProductSingletonRight(s1, s2)
      case (s1: Finite[T], s2: Finite[V]) => new ProductFinite(s1, s2)
    }

  def apply[T, V, U](s1: Finite[T], s2: Finite[V], combine: (T, V) => U): Finite[U] =
    (s1, s2) match {
      case (Empty, _) | (_, Empty) =>
        Empty
      case (s1: Singleton[T], s2: Finite[V]) =>
        Map(s2, { (v: V) => combine(s1.el, v) })
      case (s1: Finite[T], s2: Singleton[V]) =>
        Map(s1, { (v: T) => combine(v, s2.el) })
      case (s1: Finite[T], s2: Finite[V]) => new ProductFiniteComb(s1, s2)(combine)
    }

  def apply[T, V](s1: Finite[T], s2: Infinite[V]): Infinite[(T, V)] =
    new ProductFiniteInfiniteLeft(s1, s2)

//  def apply[T, V, U](s1: Singleton[T], s2: Finite[V]): Finite[(T, V)] =
//    new ProductSingleton(s1, s2)
//
//  def apply[T, V, U](s1: T, s2: Finite[V]): Finite[(T, V)] =
//    new ProductSingleton(s1, s2)
  
  def apply[T](array: Array[Finite[T]]): Finite[List[T]] =
    new ProductFiniteList(array)
  
  def apply[T, E[A] <: Enum[A]](array: Array[E[T]])(implicit ct: ClassTag[E[_]]): Enum[List[T]] =
    implicitly[ClassTag[E[_]]] match {
      case _: Finite[_] =>
        new ProductFiniteList(array.asInstanceOf[Array[Finite[T]]])
    }
  
  def fin[T](array: Array[Finite[T]]): Finite[List[T]] =
    new ProductFiniteList(array.asInstanceOf[Array[Finite[T]]])
  
  def memoized[T](array: Array[Finite[T]]): Finite[List[T]] =
    new ProductFiniteList(array.asInstanceOf[Array[Finite[T]]]) with
      scife.enumeration.memoization.MemoizedSize with
      scife.enumeration.memoization.MemoizedDynamic[List[T]]

}
