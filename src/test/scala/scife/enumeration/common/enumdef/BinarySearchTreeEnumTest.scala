package scife
package enumeration
package common
package enumdef

import scife.{ enumeration => e }
import scife.util._

import scala.language.postfixOps

import logging._

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._
import org.scalacheck.Gen

class BinarySearchTreeEnumTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
  HasLogger with ProfileLogger {
  
  import BinarySearchTreeEnum._
  
  test("Equivalence of enum definitions, with scope") {
    import e.memoization.scope._

    val memoizationScope = new AccumulatingScope  

    val enumDefList = List(
      constructEnumeratorBenchmark, constructEnumTestcase, constructBenchmarkOld
    )

    for ( enumDef <- enumDefList ) BinarySearchTreeTestHelper.testCorrectness( enumDef )

    forAll(Gen.choose(0, 8), Gen.choose(0, 20), Gen.choose(0, 20), minSuccessful(10)) {
      (size, rBeg, rEnd) =>
        val range = Range(rBeg, rEnd)
        enumDefList.zip(enumDefList.tail) foreach 
          { case (d1, d2) =>
            val e1 = d1(size, range)
            val e2 = d2(size, range)

            e1.size shouldBe e2.size
            
            for (i <- 0 until e1.size)
              e1(i) shouldBe e2(i)
          }
        
    }

  }
  
}
