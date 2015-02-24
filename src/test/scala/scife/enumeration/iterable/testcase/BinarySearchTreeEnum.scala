package scife
package enumeration
package iterable
package testcase

import scife.{ enumeration => e }
import e.iterable._
import e.dependent._

import scife.util.logging._
import scife.util._

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

import scala.language.postfixOps

class BinarySearchTreeEnum extends FunSuite with Matchers
  with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {
  
  import common._
  import enumdef.lazytest._
  import scife.util.structures._
  
  test("correctness of enumeration") {
    val enum = BinarySearchTreeEnum.constructEnumerator

    BinarySearchTreeTestHelper.testCorrectness( Depend.fin {
      in: (Int, Range) =>
        enum(in) map LazyBSTrees.toRegularBSTTree
    })

  }
  
  test("resetting enumerator in the middle") {
    val depEnum = BinarySearchTreeEnum.constructEnumerator
    val enum = depEnum(10, 1 to 10)

    val halfOfTheEnum =
      for (ind <- 0 until enum.size/2)
        yield enum(ind)
        
    enum.reset
    
    for ((el, ind) <- halfOfTheEnum.zipWithIndex)
      enum(ind) should be (el)

  } 

}
