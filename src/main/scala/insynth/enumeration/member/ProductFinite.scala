package insynth.enumeration
package member

import combinators.{ Product => CProduct }

import _root_.insynth.util
import util.Math._
import util.logging._

class ProductFinite[T, V]
	(override val left: MemberFinite[T], override val right: MemberFinite[V])
	extends lzy.ProductFinite(left, right) with MemberFinite[(T, V)] {
  
  override def member(a: (T, V)) = {
    val (leftVal, rightVal) = a
    
    left.member(leftVal) && right.member(rightVal)
  }

}