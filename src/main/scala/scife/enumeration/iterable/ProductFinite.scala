package scife.enumeration
package iterable

import combinators.{ Product => CProduct }

protected[enumeration] class ProductFinite[T, V]
  (override val left: ResetIterFinite[T], override val right: ResetIterFinite[V])
  extends scife.enumeration.lzy.ProductFinite[T, V](left, right) with ResetIter[(T, V)] {
  
  require(left.hasNext)
  require(right.hasNext)
  
  // prepare for next head call
  left.next
  
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
  
  override def reset = {
    left.reset
    right.reset
    left.next
  }

}