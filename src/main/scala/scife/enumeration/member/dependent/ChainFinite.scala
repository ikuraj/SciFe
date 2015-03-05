package scife.enumeration
package member
package dependent

import scife.{ enumeration => e }
import e.{ dependent => dp }

import scife.util._

class ChainFinite[I, O]
  (override val left: MemberFinite[I], override val right: MemberDependFinite[I, O])
  extends dp.lzy.ChainFinite[I, O](left, right) with MemberFinite[(I, O)] with HasLogger {
  
  def member(a: (I, O)) = {
    val (leftIn, rightIn) = a
    
    left.member(leftIn) && right(leftIn).member(rightIn)
  }

}