package insynth.structures

import scala.util.Random

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

// enable implicit conversions
import scala.language.implicitConversions

class DomainTypeTest extends FunSuite with ShouldMatchers {    
  
	implicit def singletonList(x: DomainType) = List(x)
	implicit def singletonTSet(x: SuccinctType) = TSet(x)
		
  val stringType = Const("String")
  val intType = Const("Int") 
  
  val stringAtom = Atom(stringType)
  val intAtom = Atom(intType)
  
  val fun1 = Function(stringAtom, intAtom)
  val fun2 = Function(intAtom, stringAtom)
  
  val funFun1 = Function(stringAtom, fun2)
  val funFun2 = Function(List(fun1, fun2), intAtom)
  val funFun3 = Function(List(fun1, fun2), fun2)
  
  test("transform to succinct type of simple types") {
    stringAtom.toSuccinctType should be (stringType)
    intAtom.toSuccinctType should be (intType)
  }
  
  test("transform to succinct type of function types") {
    fun1.toSuccinctType should be (Arrow(stringType, intType))
    fun2.toSuccinctType should be (Arrow(intType, stringType))
  }
  
  test("transform to succinct type of function types with function arguments") {
    funFun1.toSuccinctType should be (Arrow(TSet(stringType, intType), stringType))
    funFun2.toSuccinctType should be (Arrow(TSet(Arrow(stringType, intType), Arrow(intType, stringType)), intType))
    funFun3.toSuccinctType should be (Arrow(TSet(Arrow(stringType, intType), Arrow(intType, stringType), intType), stringType))
  }
  
}