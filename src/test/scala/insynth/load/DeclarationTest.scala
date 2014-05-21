package insynth.load

import org.scalatest._

import insynth.testdomain._
import insynth.common._

class DeclarationTest extends FunSuite with Matchers {
  
  import CommonDeclarations._
  val dt = CommonDomainTypes
  val st = CommonSuccinctTypes
  
  test("declarations build from domain types and corresponding sucinct types should match") {
  	booleanDeclaration should equal(TestDeclaration(dt.typeBoolean, st.typeBoolean))
  	intDeclaration should equal(TestDeclaration(dt.typeInt, st.typeInt))   
  	unitDeclaration should equal(TestDeclaration(dt.typeUnit, st.typeUnit))
  
  	functionBoolToIntDeclaration should equal(TestDeclaration(dt.functionBoolToIntType, st.functionBoolToIntType))
  	functionFun1ToUnitDeclaration should equal(TestDeclaration(dt.functionFun1ToUnitType, st.functionFun1ToUnitType))
  	functionIntToIntDeclaration should equal(TestDeclaration(dt.functionIntToIntType, st.functionIntToIntType))
  	
  	functionFunsToFunDeclaration should equal(TestDeclaration(dt.functionFunsToFunType, st.functionFunsToFunType))
  }
  
  test("declarations should have default weight 1.0f") {
  	booleanDeclaration should equal(TestDeclaration(dt.typeBoolean, st.typeBoolean, 1f))
  }

  test("declarations created through factory methods should be the same") {
    TestDeclaration(dt.typeInt) should equal (TestDeclaration(dt.typeInt))
    TestDeclaration(dt.typeInt, "name") should equal (TestDeclaration(dt.typeInt, "name"))
  }  
}