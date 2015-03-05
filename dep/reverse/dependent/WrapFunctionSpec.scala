package insynth
package enumeration
package reverse
package dependent

import org.scalatest._

class WrapFunctionSpec extends WordSpec with Matchers {

  "A WrapFunction Reverse" when {
    "constructed in normal fashion" should {

      val reverse = new WrapFunction(
        { (x: Int) =>
          new WrapArray(Array(1 to x: _*))
        })

      "enumerate correct enumerators" in {
        reverse(0).size should be (0)
        reverse(5).size should be (5)
      }

    }

    "constructed in recursive fashion" should {

      val reverse = new WrapFunction(
        { (dr: ReverseDepend[Int, Int], x: Int) =>
          new WrapArray(Array(1 to x: _*))
        })

      "enumerate correct enumerators" in {
        reverse(0).size should be (0)
        reverse(5).size should be (5)
      }

    }
  }
}