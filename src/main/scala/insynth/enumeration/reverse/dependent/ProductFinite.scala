package insynth.enumeration
package reverse
package dependent

import insynth.util.logging._

case class ProductFinite[I, O1, O2]
  (s1: ReverseDependFinite[I, O1], s2: ReverseDependFinite[I, O2])
  extends ReverseDependFinite[I, (O1, O2)] {
  
  override type EnumType = ReverseFinite[(O1, O2)]
  
  def getEnum(parameter: I) = {
    val e1 = s1.getEnum(parameter)
    val e2 = s2.getEnum(parameter)
    
    new reverse.ProductFinite( e1, e2 )
  }
    
}