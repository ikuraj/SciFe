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

import java.util.{ Date => JDate, _ }
import java.text.DateFormat

import scife.util.logging._

import scala.language.existentials
import scala.language.implicitConversions

import org.scalatest._
import org.scalatest.prop._
import org.scalameter.api._

class DateExampleTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger {
  import Util.CheckerHelper
  import Checks._

  // import DSL
  import e._
  import Enum._

  // import helper classes/methods
  import DateExampleTest._

  test("Dates example") {

    val dates = (1 to 31) ⊗ (1 to 12) ⊗ Stream.from(2015) ≻ {
      isValid(_)
    } ↑ { case ((d, m), y) ⇒ Date(y, m, d) }

    val defSizeFlag = dates.hasDefiniteSize // returns false
    defSizeFlag shouldBe false

    for (i ← 3 to 365 by 7)
      info(df.format(dates(i): JDate)) // Sundays in 2015
//      println(df.format(dates(i): JDate)) // Sundays in 2015

    val oopslaDate = dates(298) // enumerate start date of OOPSLA 2015
    cal.setTime(oopslaDate)
    val oopslaJDate = cal.getTime
    cal.get(Calendar.YEAR) should be(2015)
    cal.get(Calendar.DAY_OF_MONTH) should be(25)
    // months start from 0
    cal.get(Calendar.MONTH) should be(9)
  }

}

object DateExampleTest extends HasLogger {
  // a calendar object for manipulation of dates
  implicit val cal = Calendar.getInstance()
  cal.setLenient(false)
  val df = DateFormat.getDateInstance()
  
  type JDate = java.util.Date
  
  // Java Date constructor is deprecated--we will use a helper class to construct Date objects
  case class Date(y: Int, m: Int, d: Int)
  
  // Java calendar months are 0 .. 11
  implicit def dateToJavaDate(d: Date)(implicit cal: Calendar) = {
    info("Date given: " + d)
    cal.set(d.y, d.m - 1, d.d)
    cal.getTime
  }

  def isValid(date: ((Int, Int), Int))(implicit cal: Calendar) = date match {
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
  
}
