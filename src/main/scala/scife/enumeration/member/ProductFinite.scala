package scife.enumeration
package member

import combinators.{ Product => CProduct }

import _root_.scife.util
import util.Math._
import util.logging._

class ProductFinite[T, V](override val left: MemberFinite[T], override val right: MemberFinite[V])
  extends lzy.ProductFinite(left, right) with MemberFinite[(T, V)] {

  override def member(a: (T, V)) = {
    val (leftVal, rightVal) = a

    left.member(leftVal) && right.member(rightVal)
  }

}

//optimization class
// TODO
//protected[enumeration] class ProductFiniteComb[T, V, U]
//  (override val left: MemberFinite[T], override val right: MemberFinite[V])(combine: (T, V) => U)
//  extends Finite[U] with HasLogger {
//
//  override def size = left.size * right.size
//
//  override def apply(ind: Int) = {
//    val i1 = ind % left.size
//    val i2 = ind / left.size
//    combine(left(i1), right(i2))
//  }
//
//}

class ProductSingleton[T, V](el: T, override val right: MemberFinite[V])
  extends lzy.ProductSingleton[T, V](el, right) with MemberFinite[(T, V)] with HasLogger {

  def this(singleton: Singleton[T], right: MemberFinite[V]) = this(singleton.el, right)

  override def member(a: (T, V)) = {
    val (leftVal, rightVal) = a

    el == leftVal && right.member(rightVal)
  }

}

class ProductSingletonRight[T, V](override val left: MemberFinite[V], el: T)
  extends lzy.ProductSingletonRight[T, V](left, el) with MemberFinite[(V, T)] with HasLogger {

  def this(left: MemberFinite[V], singleton: Singleton[T]) = this(left, singleton.el)

  override def member(a: (V, T)) = {
    val (leftVal, rightVal) = a

    el == rightVal && left.member(leftVal)
  }

}