package scife.enumeration
package iterable

import combinators.{ Product => CProduct }

protected[enumeration] class ProductFinite[T, V]
  (override val left: ResetIterFinite[T], override val right: ResetIterFinite[V])
  extends lzy.ProductFinite[T, V](left, right) with ResetIter[(T, V)] {
  
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