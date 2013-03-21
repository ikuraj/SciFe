package insynth.common

import insynth.structures._

object CommonDomainTypes {

	val typeInt = Atom(Const("Int"))
	val typeString = Atom(Const("String"))
	val typeBoolean = Atom(Const("Boolean"))
	val typeUnit = Atom(Const("Unit"))
                
  val functionBoolToIntType =
    Function(List(typeBoolean), typeInt)
  
  val functionFun1ToUnitType =
    Function(List(typeUnit, typeInt), typeBoolean)
   
  val functionIntToIntType =
    Function(List(typeInt), typeInt)
    
  val threeParFunctionType =
    Function(List(typeInt, typeInt, typeBoolean), typeInt)

  val functionFunsToFunType =
    Function(List(functionBoolToIntType, functionFun1ToUnitType), functionIntToIntType)
    
}