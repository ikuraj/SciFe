package scife
package enumeration
package iterable

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
}
