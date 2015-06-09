package scife.enumeration
package iterable

import combinators.{ Product => CProduct }

import scalaz.LazyTuple2

protected[enumeration] class ProductFinite[T, V]
  (override val left: ResetIterFinite[T], override val right: ResetIterFinite[V])
  extends scife.enumeration.lzy.ProductFinite[T, V](left, right) with ResetIter[(T, V)] {
  
//  override def apply(ind: Int) = throw new RuntimeException
  
  require(left.hasNext)
  require(right.hasNext)
  
  // prepare for next head call
  var currLeft = left.next
  
  override def next = {
    if (!right.hasNext) {
      right.reset
      currLeft = left.next
    }

    
//    left.head
    (currLeft, right.next)
//    constructCurrent
  }
  
//  private[this] def constructCurrent = (left.head, right.head)
//  
  override def head =
    throw new RuntimeException
//    constructCurrent
  
  override def hasNext = {
    right.hasNext || left.hasNext
  }
  
  override def reset = {
    left.reset
    right.reset
    currLeft = left.next
  }

}

protected[enumeration] class ProductFiniteLazyTuple[T, V]
  (val left: ResetIterFinite[T], val right: ResetIterFinite[V])
  extends Finite[LazyTuple2[T, V]] with ResetIter[LazyTuple2[T, V]] {
  
  override def apply(ind: Int) =
    throw new RuntimeException
//  {
//    entering("apply", ind)
//    val i1 = ind % left.size
//    val i2 = ind / left.size
//    LazyTuple2(left(i1), right(i2))
//  }

  require(left.hasNext)
  require(right.hasNext)
  
  // prepare for next head call
  var currLeft: T =
    left.next
  
  override def next = {
    require(hasNext,
      s"left $left:${left.hashCode()} has no next" + s"right ${right}:${right.hashCode()} has no next"
    )
    if (!right.hasNext) {
      right.reset
      currLeft = left.next
    }

    LazyTuple2(currLeft, right.next)
//    right.next
//    constructCurrent
  }
  
  private[this] def constructCurrent = 
    LazyTuple2(left.head, right.head)
  
  override def hasNext = {
    right.hasNext || left.hasNext
  }
  
  override def reset = {
    left.reset
    right.reset
    left.next
  }
  
  override def head =
    throw new RuntimeException
//    constructCurrent

  override def size = left.size * right.size
  
}

class ProductSingleton[T, V]
  (el: T, val right: ResetIterFinite[V])
  extends Finite[LazyTuple2[T, V]] with ResetIter[LazyTuple2[T, V]] {
  
  require(right.hasNext)
  
  // prepare for next head call
//  right.next
  
  override def next = {
    assert(right.hasNext)
    LazyTuple2(el, right.next)
  }
  
  override def hasNext = {
    right.hasNext
  }
  
  override def reset = {
    right.reset
  }

  override def size = right.size

  def this(singleton: Singleton[T], right: ResetIterFinite[V]) = this(singleton.el, right)

  override def apply(ind: Int) = {
    LazyTuple2(el, right(ind))
  }

}

class ProductSingletonRight[T, V]
  (val left: ResetIterFinite[V], el: T)
  extends Finite[LazyTuple2[V, T]] with ResetIter[LazyTuple2[V, T]] {
  
  require(left.hasNext)
  
  // prepare for next head call
//  left.next
  
  override def next = {
    assert(left.hasNext)
    LazyTuple2(left.next, el)
  }
  
  override def hasNext = {
    left.hasNext
  }
  
  override def reset = {
    left.reset
  }

  override def size = left.size

  def this(left: ResetIterFinite[V], singleton: Singleton[T]) = this(left, singleton.el)

  override def apply(ind: Int) = {
    LazyTuple2(left(ind), el)
  }

}