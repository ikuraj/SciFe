package insynth.enumeration
package lzy

import insynth.enumeration.{ combinators => comb }

import _root_.insynth.util
import util.Math._

trait ProductFiniteInfinite[T, V, U <: Pair[_, _]] extends Infinite[U]  {
  
  val finite: Finite[T]
  val infinite: Infinite[V]
  
  def makePair(finEl: T, infEl: V): U
  
  override def apply(ind: Int) = {
    val i1 = ind % finite.size
    val i2 = ind / finite.size
    makePair( finite(i1), infinite(i2) )
  }
  
}

protected[enumeration] class ProductFiniteInfiniteLeft[T, V]
	(override val left: Finite[T], override val right: Infinite[V])
	extends ProductFiniteInfinite[T, V, (T, V)]
	with comb.Product[T, V] with Infinite[(T, V)] with HasLogger {
  
  override def size =
    super[Infinite].size

  override val finite = left
  override val infinite = right
  
  def makePair(finEl: T, infEl: V) = (finEl, infEl)
  
}

protected[enumeration] class ProductFiniteInfiniteRight[T, V]
	(override val left: Infinite[T], override val right: Finite[V])
	extends ProductFiniteInfinite[V, T, (T, V)]
	with comb.Product[T, V] with Infinite[(T, V)] with HasLogger {
  
  override def size =
    super[Infinite].size
  
  override val finite = right
  override val infinite = left
  
  def makePair(finEl: V, infEl: T) = (infEl, finEl)
  
}

// for optimization
protected[enumeration] class ProductFiniteInfiniteComb[T, V, U]
	(val left: Finite[T], val right: Infinite[V])
	(combine: (T, V) => U)
	extends Infinite[U] with HasLogger {
  
  override def apply(ind: Int) = {
    val i1 = ind % left.size
    val i2 = ind / left.size
    combine( left(i1), right(i2) )
  }
  
}