package insynth
package enumeration
package reverse
package dependent

import org.scalatest._

class InMapSpec extends WordSpec with Matchers {

  "A InMap Reverse" should {

    val reverse = new InMap( 
      new WrapFunction(
        { (x: Int) =>
          new WrapArray(Array(1 to x: _*))
        }), { (_: Int) + 1 }
      )

    "enumerate enumerators of correct type" in {
      reverse(0) shouldBe a [Reverse[_]]
    }

  }
}