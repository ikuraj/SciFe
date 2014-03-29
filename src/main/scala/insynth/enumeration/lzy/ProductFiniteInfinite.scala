package insynth.enumeration
package lzy

import combinators.Product

import _root_.insynth.util
import util.Math._
import util.logging._

protected[enumeration] class ProductFiniteInfinite[T, V]
	(override val left: Finite[T], override val right: Infinite[V])
	extends Product[T, V] with Infinite[(T, V)] with HasLogger {
  
  override def size =
    super[Infinite].size
  
  override def apply(ind: Int) = {
    val i1 = ind % left.size
    val i2 = ind / left.size
    ( left(i1), right(i2) )
  }
  
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