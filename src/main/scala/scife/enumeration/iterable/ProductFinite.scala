package scife.enumeration
package iterable

import combinators.{ Product => CProduct }

protected[enumeration] class ProductFinite[T, V]
  (override val left: Finite[T] with ResetIter[T], override val right: Finite[V] with ResetIter[V])
  extends lzy.ProductFinite[T, V](left, right) with ResetIter[(T, V)] with HasLogger {
  
  override def next = {
    if (!right.hasNext) {
      right.reset
      left.next
    }

    (left.head, right.next)
  }
  
  override def hasNext = {
    right.hasNext || left.hasNext
  }

}