package insynth.enumeration
package reverse

import combinators.{ Product => CProduct }

import _root_.insynth.util
import util.Math._
import util.logging._

class ProductFinite[T, V]
	(override val left: Reverse[T], override val right: Reverse[V])
	extends lzy.ProductFinite(left, right) with Reverse[(T, V)] {
  
  override def reverse[B >: (T, V)](a: B) = {
    val (leftVal, rightVal) = a.asInstanceOf[(T, V)]
    val leftInd = left.reverse(leftVal)
    val rightInd = right.reverse(rightVal)
    
    rightInd * left.size + leftInd
  }

}