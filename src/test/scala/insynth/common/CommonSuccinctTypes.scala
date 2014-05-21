package insynth.common

import insynth.structures._

object CommonSuccinctTypes {

	val typeInt = Const("Int")
	val typeString = Const("String")
	val typeBoolean = Const("Boolean")
	val typeUnit = Const("Unit")
        
  val functionBoolToIntType =
    Arrow(TSet(typeBoolean), typeInt)
      
  val functionFun1ToUnitType =
    Arrow(TSet(typeUnit, typeInt), typeBoolean)
      
  val functionIntToIntType =
    Arrow(TSet(typeInt), typeInt)
    
  val functionFunsToFunType =
    Arrow(TSet(functionBoolToIntType, functionFun1ToUnitType, typeInt), typeInt)
   
}