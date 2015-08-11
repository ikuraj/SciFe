package scife
package enumeration
package showcase
package oopsla15

import dependent._
import memoization._
import scife.{ enumeration => e }

import scife.util._
import structures._
import BSTrees._

import scife.util.logging._

import scala.language.existentials
import scala.language.implicitConversions

import org.scalatest._
import org.scalatest.prop._
import org.scalameter.api._

class NQueen extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {
  import common._
  import Util.CheckerHelper
  import Checks._

  // import DSL
  import e._
  import Enum._

  type Queen = (Int,Int)

  test("Simple enumeration") {

    val qs = (1 to 8) ⊗ (1 to 8)

    // we do not want to invoke implicit conversion here and construct lists
    // (that would yield many more unecessary conversions and iterations when constructing qss)
    val mkq = //for (q <- qs) yield q
      qs

    val qss = mkq ⊗ mkq ⊗ mkq ⊗ mkq ↑ { 
      case (((q1,q2),q3),q4) ⇒ List(q1,q2,q3,q4) }

    qss.size shouldBe 16777216
    
    println(qss(1))
  }
  
  test("Greater size") {

    val mkq = (1 to 16) ⊗ (1 to 16)

    val qss = mkq ⊗ mkq ⊗ mkq ⊗ mkq ↑ { 
      case (((q1,q2),q3),q4) ⇒ List(q1,q2,q3,q4) }
    
    println(qss(1))
    println(qss(scala.util.Random.nextInt(Int.MaxValue)))
    println(qss(Int.MaxValue))
  }

}
