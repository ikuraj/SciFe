package insynth.enumeration
package reverse

import combinators.{ Product => CProduct }

import _root_.insynth.util
import util.Math._
import util.logging._

class ProductFinite[I, T, V]
	(val left: Reverse[I, T], val right: Reverse[I, V])
	extends lzy.ProductFinite(left, right) with Reverse[I, (T, V)] {
  
  override def reverse[B >: (T, V)](a: B, par: I) = {
    val (leftVal, rightVal) = a.asInstanceOf[(T, V)]
    val leftInd = left.reverse(leftVal, par)
    val rightInd = right.reverse(rightVal, par)
    
    new lzy.ProductFinite(left, right) with Reversed[(T, V)] {
      override val pos = rightInd * left.size + leftInd
    }
  }

}