package insynth.enumeration
package reverse

import combinators.{ Product => CProduct }

import _root_.insynth.util
import util.Math._
import util.logging._

class ProductFinite[T, V]
	(override val left: ReverseFinite[T], override val right: ReverseFinite[V])
	extends lzy.ProductFinite(left, right) with ReverseFinite[(T, V)] {
  
  override def reverse(a: (T, V)) = {
    val (leftVal, rightVal) = a
    val leftInd = left.reverse(leftVal)
    val rightInd = right.reverse(rightVal)
    
    rightInd * left.size + leftInd
  }

}