package scife.enumeration
package lzy

import combinators.{ Product => CProduct }

import _root_.scife.util
import util.Math._

import scife.util._

protected[enumeration] class ProductFinite[T, V]
  (override val left: Finite[T], override val right: Finite[V])
  extends CProduct[T, V] with Finite[(T, V)] with HasLogger {

  override def apply(ind: Int) = {
    val i1 = ind % left.size
    val i2 = ind / left.size
    (left(i1), right(i2))
  }

}

// optimization class
protected[enumeration] class ProductFiniteComb[T, V, U]
  (val left: Finite[T], val right: Finite[V])
  (combine: (T, V) => U)
  extends Finite[U] with HasLogger {

  override def size = left.size * right.size

  override def apply(ind: Int) = {
    val i1 = ind % left.size
    val i2 = ind / left.size
    combine(left(i1), right(i2))
  }

}

class ProductSingleton[T, V]
  (el: T, val right: Finite[V])
  extends Finite[(T, V)] with HasLogger {

  override def size = right.size

  def this(singleton: Singleton[T], right: Finite[V]) = this(singleton.el, right)

  override def apply(ind: Int) = {
    (el, right(ind))
  }

}

class ProductSingletonRight[T, V]
  (val left: Finite[V], el: T)
  extends Finite[(V, T)] with HasLogger {

  override def size = left.size

  def this(left: Finite[V], singleton: Singleton[T]) = this(left, singleton.el)

  override def apply(ind: Int) = {
    (left(ind), el)
  }

}

protected[enumeration] class ProductFiniteList[T](val enums: Array[Finite[T]])
  extends Finite[List[T]] with HasLogger {
  
  val sizes = enums.map(_.size)

  override def apply(ind: Int) = {
    val buffer = scala.collection.mutable.ListBuffer.empty[T]
    var currInd = ind
    var currEnumInd = 0
    while (currEnumInd < enums.size) {
      buffer += enums(currEnumInd)(currInd % sizes(currEnumInd))
      currInd /= sizes(currEnumInd)
      currEnumInd += 1
    }
    
    buffer.toList
  }
  
  override def size = sizes.reduce(_ * _)

}
