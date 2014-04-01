package insynth.enumeration
package dependent

import insynth.enumeration.{ combinators => ecomb }

import insynth.util.logging._

object Product {
  
  def apply[I, O1, O2](s1: Depend[I, O1], s2: Depend[I, O2]) = {
    (s1, s2) match {
      case (df1: DependFinite[I, O1], df2: DependFinite[I, O2]) =>
      	new ProductFinite(df1, df2)
    }
  }
  
}

// TODO refactor to make these factories more modular
//trait Product[I, O1, O2] {
//  
//  type FactoryType <: {
//    def apply[EnumType1 <: Enum[O1], EnumType2 <: Enum[O2]]
//  		(e1: EnumType1, e2: EnumType2)
//  }
//
//  val factory: FactoryType
//  
//  val s1: Depend[I, O1]
//  val s2: Depend[I, O2]
//  
//  def getEnum(parameter: I) = {
//    val e1 = s1.getEnum(parameter)
//    val e2 = s2.getEnum(parameter)
//    
//    factory( e1, e2 )
//  }
//  
//}