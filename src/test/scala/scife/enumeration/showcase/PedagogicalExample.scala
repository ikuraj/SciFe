package scife
package enumeration
package showcase

import dependent._
import memoization._
import scife.{ enumeration => e }

import scife.util._
import Structures._
import BSTrees._

import scife.util.logging._

import scala.language.existentials
import scala.language.implicitConversions

import org.scalatest._
import org.scalatest.prop._
import org.scalameter.api._

class PedagogicalExample extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {
    import Util.CheckerHelper
  import Checks._

  // import DSL
  import e._
  import Enum._

  test("Dates example") {
    // Java Date constructor is deprecated...
    case class Date(y: Int, m: Int, d: Int)

    import java.util._
    val cal = Calendar.getInstance()
    cal.setLenient(false)

    // Java calendar months are 0 .. 11
    implicit def dateToJavaDate(d: Date) = {
      info("Date given: " + d)
      cal.set(d.y, d.m - 1, d.d)
      cal.getTime
    }

    def isValid(date: ((Int, Int), Int)) = date match {
      case ((d, m), y) =>
        try {
          cal.set(y, m - 1, d)
          cal.getTime
          true
        } catch {
          case _: Exception =>
            fine("Invalid date filtered: " + date)
            false
        }
    }

    val dates = (1 to 31) ⊗ (1 to 12) ⊗ Stream.from(2014) ≻ {
      isValid(_)
    } ↑ { case ((d, m), y) ⇒ new Date(y, m, d) }

    val comb = (1 to 31) ⊗ (1 to 12) ⊗ Stream.from(2014) ≻ { isValid(_) }

    comb shouldBe a [eager.SimpleFilter[_]]
    comb(209) should be ((28, 7), 2014)
    for (i <- 0 until 400)
      info("comb(%d) is %s".format(i, comb(i)))

    for (i <- 0 until 400)
      withClue((dates(i): java.util.Date).toString) {
        noException should be thrownBy
          cal.setTime(dates(i))
      }

    cal.setTime(dates(209))
    val scala2014StartDate = cal.getTime
    cal.get(Calendar.YEAR) should be (2014)
    cal.get(Calendar.DAY_OF_MONTH) should be (28)
    // months start from 0
    cal.get(Calendar.MONTH) should be (6)
  }

  // paper
  test("allConvenientlyDivisblePairs") {
    val allConvenientlyDivisblePairs =
      (List((1, 1), (3, 3)) ⊕
        ((1 to 9) ** (Stream.from(0) filter { _ % 5 == 0 })) map
          { case (x, y) => ("valid", x, y) })
  }

  test("allConvenientlyDivisblePairs2") {
    (Enum((1, 1), (3, 3)): Enum[(Int, Int)]) ⊕ (Enum(1 to 9)
      product (Stream.from(0) filter { _ % 5 == 0 })) ↑ {
        case (x, y) => ("valid", x, y) }
  }

  test("lazyRecursiveEnumeration") {
    //    val enum = rec(self, ind) {
    //      Enum(2, 3) concat
    //        inmap(ind - 2)self 
    //    }
    //
    //    val res: Enum[(String, ((Int, Int), Int))] =
    //    ((Enum(1 to 31) ** Enum(1 to 12) ** Enum(Stream.from(2014)) ⊘ {
    //      _ => true; // whether its a good year
    //    }): Enum[((Int, Int), Int)]) ↑ { case p@((x, y), z) => if (true) ("leap", p) else ("no", p) }
    //    
    //
    //    val res2 =
    //    (1 to 31) ⊗ (1 to 12) ⊗ Stream.from(2014) ⊘ {
    //      case ((d, m), y) => true } ↑ { ("leap", _) }
    //    
    //    
    //    // lazy fibonacci are doable!
    //    val fibs: Enum[Int] = (Enum(0, 1): Enum[Int])
    //    val fibs2: Enum[Int] = (Enum(Stream.from(0)).map{ (i: Int) => fibs(i-1) + fibs(i-2) })
    //    val fib = fibs ++ fibs2
  }

}
