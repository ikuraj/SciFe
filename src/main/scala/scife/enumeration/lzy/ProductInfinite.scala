package scife.enumeration
package lzy

import _root_.scife.util
import util.Math._

// Cantor inverse mapping is fine in this case
protected[enumeration] class ProductInfinite[T, V]
  (override val left: Infinite[T], override val right: Infinite[V])
  extends combinators.Product[T, V] with Infinite[(T, V)] with HasLogger {

  override def apply(ind: Int) = {
    val (i1, i2) = cantorInverse(ind)
    ( left(i1), right(i2) )
  }

}

// optimization
protected[enumeration] class ProductInfiniteComb[T, V, U]
  (val left: Infinite[T], val right: Infinite[V])
  (combine: (T, V) => U)
  extends Infinite[U] with HasLogger {

  override def apply(ind: Int) = {
    val (i1, i2) = cantorInverse(ind)
    combine( left(i1), right(i2) )
  }

}
