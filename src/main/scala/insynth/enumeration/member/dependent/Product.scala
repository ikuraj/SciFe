package insynth.enumeration
package member
package dependent

import insynth.enumeration.{ combinators => ecomb }

import insynth.util.logging._

object Product {
  
  def apply[I, O1, O2](s1: MemberDependFinite[I, O1], s2: MemberDependFinite[I, O2]) = {
  	new ProductFinite(s1, s2)
  }
  
}