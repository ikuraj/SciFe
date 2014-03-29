package insynth.enumeration
package lzy

import _root_.insynth.util
import util.Math._
import util.logging._

object Product {
  
	def apply[T, V, U](s1: Enum[T], s2: Enum[V]) =
	  (s1, s2) match {
	  	case (s1: Finite[T], s2: Finite[V]) => new ProductFinite(s1, s2)
	  	case (s1: Infinite[T], s2: Infinite[V]) => new ProductInfinite(s1, s2)
	  	case (s1: Finite[T], s2: Infinite[V]) => new ProductFiniteInfinite(s1, s2)
      case (Empty, _) | (_, Empty) =>
        Empty
//      case (_, _) if s1.hasDefiniteSize && s2.hasDefiniteSize =>
//        new ProductFinite(s1, s2)( (_, _) )
//	  	case _ => throw new RuntimeException
		}
  
	def apply[T, V, U](s1: Enum[T], s2: Enum[V], combine: (T, V) => U) =
	  (s1, s2) match {
	  	case (s1: Finite[T], s2: Finite[V]) => new ProductFiniteComb(s1, s2)(combine)
	  	case (s1: Infinite[T], s2: Infinite[V]) => new ProductInfiniteComb(s1, s2)(combine)
	  	case (s1: Finite[T], s2: Infinite[V]) => new ProductFiniteInfiniteComb(s1, s2)(combine)
	  	case _ => throw new RuntimeException
		}
  
//	def apply[T, V, U](s1: Finite[T], s2: Finite[V])(combine: (T, V) => U) =
//	  new ProductFinite(s1, s2)(combine)
//  
//	def apply[T, V, U](s1: Infinite[T], s2: Infinite[V])(combine: (T, V) => U) =
//	  new ProductInfinite(s1, s2)(combine)
//  
//	def apply[T, V, U](s1: Finite[T], s2: Infinite[V])(combine: (T, V) => U) =
//	  new ProductOneFinite(s1, s2)(combine)
	  
}