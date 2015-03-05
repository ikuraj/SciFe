package scife
package enumeration
package member
package dependent

import org.scalatest._

class WrapFunctionSpec extends WordSpec with Matchers {

  "A WrapFunction Member" when {
    "constructed in normal fashion" should {

      val member = new WrapFunction(
        { (x: Int) =>
          new WrapArray(Array(1 to x: _*))
        })

      "enumerate correct enumerators" in {
        member(0).size should be (0)
        member(5).size should be (5)
        member(5) shouldBe a [Member[_]]
      }

    }

    "constructed in recursive fashion" should {

      val member = new WrapFunction(
        { (dr: MemberDepend[Int, Int], x: Int) =>
          new WrapArray(Array(1 to x: _*))
        })

      "enumerate correct enumerators" in {
        member(0).size should be (0)
        member(5).size should be (5)
        member(5) shouldBe a [Member[_]]
      }

    }
  }
}