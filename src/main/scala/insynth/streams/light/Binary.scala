package insynth
package streams.light

import util.logging._
import util.Math._

// Cantor inverse mapping is fine in this case
protected[streams] class BinaryInfinite[T, V, U](val s1: Infinite[T], val s2: Infinite[V])
	(combine: (T, V) => U)
	extends Infinite[U] with HasLogger with CountedLogger {
  
  override def apply(ind: Int) = {
    val (i1, i2) = cantorInverse(ind)
    combine(
      s1(i1), s2(i2)
    )
  }
  
}

protected[streams] class BinaryOneFinite[T, V, U](val s1: Finite[T], val s2: Infinite[V])
	(combine: (T, V) => U)
	extends Infinite[U] with HasLogger with CountedLogger {
  
  override def apply(ind: Int) = {
    val i1 = ind % s1.size
    val i2 = ind / s1.size
    combine(
      s1(i1), s2(i2)
    )
  }
  
}

protected[streams] class BinaryFinite[T, V, U](val s1: Finite[T], val s2: Finite[V])
	(combine: (T, V) => U)
	extends Finite[U] with HasLogger with CountedLogger {
  
  override def size = s1.size * s2.size
  
  override def apply(ind: Int) = {
    val i1 = ind % s1.size
    val i2 = ind / s1.size
    combine(
      s1(i1), s2(i2)
    )
  }
  
}

object Binary{
  
	def apply[T, V, U](s1: Enum[T], s2: Enum[V]) =
	  (s1, s2) match {
	  	case (s1: Finite[T], s2: Finite[V]) => new BinaryFinite(s1, s2)( (_, _) )
	  	case (s1: Infinite[T], s2: Infinite[V]) => new BinaryInfinite(s1, s2)( (_, _) )
	  	case (s1: Finite[T], s2: Infinite[V]) => new BinaryOneFinite(s1, s2)( (_, _) )
      case (Empty, _) => Empty
      case (_, Empty) => Empty
	  	case _ => throw new RuntimeException
		}
  
	def apply[T, V, U](s1: Enum[T], s2: Enum[V], combine: (T, V) => U) =
	  (s1, s2) match {
	  	case (s1: Finite[T], s2: Finite[V]) => new BinaryFinite(s1, s2)(combine)
	  	case (s1: Infinite[T], s2: Infinite[V]) => new BinaryInfinite(s1, s2)(combine)
	  	case (s1: Finite[T], s2: Infinite[V]) => new BinaryOneFinite(s1, s2)(combine)
	  	case _ => throw new RuntimeException
		}
  
//	def apply[T, V, U](s1: Finite[T], s2: Finite[V])(combine: (T, V) => U) =
//	  new BinaryFinite(s1, s2)(combine)
//  
//	def apply[T, V, U](s1: Infinite[T], s2: Infinite[V])(combine: (T, V) => U) =
//	  new BinaryInfinite(s1, s2)(combine)
//  
//	def apply[T, V, U](s1: Finite[T], s2: Infinite[V])(combine: (T, V) => U) =
//	  new BinaryOneFinite(s1, s2)(combine)
	  
}