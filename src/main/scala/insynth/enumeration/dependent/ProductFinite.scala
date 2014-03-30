package insynth
package enumeration
package dependent

import enumeration.{ lzy => elzy }

import insynth.util.logging._

case class ProductFinite[I, O1, O2]
  (s1: DependFinite[I, O1], s2: DependFinite[I, O2])
  extends DependFinite[I, (O1, O2)] {
  
  override type EnumType = Finite[(O1, O2)]
  
  def getEnum(parameter: I) = {
    val e1 = s1.getEnum(parameter)
    val e2 = s2.getEnum(parameter)
    
    elzy.Product( e1, e2 )
  }
    
}