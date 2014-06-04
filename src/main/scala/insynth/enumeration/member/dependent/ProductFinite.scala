package insynth.enumeration
package member
package dependent

import insynth.util.logging._

case class ProductFinite[I, O1, O2]
  (s1: MemberDependFinite[I, O1], s2: MemberDependFinite[I, O2])
  extends MemberDependFinite[I, (O1, O2)] {
  
  override type EnumType = MemberFinite[(O1, O2)]
  
  def getEnum(parameter: I) = {
    val e1 = s1.getEnum(parameter)
    val e2 = s2.getEnum(parameter)
    
    new member.ProductFinite(e1, e2)
  }
    
}