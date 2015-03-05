package insynth
package enumeration
package reverse
package dependent

import org.scalatest._

class ReverseDependSpec extends WordSpec with Matchers {

  val reverse = new WrapFunction(
    { (x: Int) =>
      new WrapArray(Array(1 to x: _*))
    })

  "A depend Reverse" should {

    "enumerate enumerators with appropriate type" in {
      reverse(0) shouldBe a [Reverse[Int]]
    }

  }

}