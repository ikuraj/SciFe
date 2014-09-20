package scife.enumeration
package reverse

import scife.{ enumeration => e }
import reverse.{ dependent => rd }
import memoization._
import e.dependent._

import org.scalatest._

import scala.language.postfixOps

class ReverseTest extends FunSuite with Matchers {

  test("factory method, normal enum") {

    {
      val reverse = Reverser( (1 to 5) toList )
      reverse.size should be (5)
      reverse.toList should be (1 to 5)
    }

    {
      val reverse = Reverser[List[Int]](List.empty[Int]): Reverse[List[Int]]
      reverse.size should be (1)
      reverse.toList should contain only (Nil)
    }

  }

  test("factory method, reverse") {

    {
      val reverse = Reverser( (1 to 5) toList )
      reverse.size should be (5)
      for (revInd <- 1 to 5)
        reverse.reverse(revInd) should be (revInd - 1)
    }

    {
      val reverse = Reverser[List[Int]](List.empty[Int]): Reverse[List[Int]]
      reverse.size should be (1)
      reverse.reverse(Nil) should be (0)
      intercept[IllegalArgumentException] {
        reverse.reverse(List(1))
      }
    }

  }

}
