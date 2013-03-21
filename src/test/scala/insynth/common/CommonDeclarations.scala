package insynth.common

import insynth.structures._

import insynth.testdomain._

object CommonDeclarations {
  
  import CommonDomainTypes._

  val booleanDeclaration = TestDeclaration(typeBoolean)  
  val intDeclaration = TestDeclaration(typeInt)      
  val unitDeclaration = TestDeclaration(typeUnit)
  
  val functionBoolToIntDeclaration = TestDeclaration(
    functionBoolToIntType
  )
  
  val functionFun1ToUnitDeclaration = TestDeclaration(
    functionFun1ToUnitType
  )
   
  val functionIntToIntDeclaration = TestDeclaration(
    functionIntToIntType
  )
  
  val threeParFunctionDeclaration = TestDeclaration(     
    threeParFunctionType
  )
  
  val functionFunsToFunDeclaration = TestDeclaration(
    functionFunsToFunType
  )

}