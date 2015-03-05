package scife
package enumeration
package member
package dependent

import org.scalatest._

class InMapSpec extends WordSpec with Matchers {

  "A InMap Member" should {

    val member = new InMap( 
      new WrapFunction(
        { (x: Int) =>
          new WrapArray(Array(1 to x: _*))
        }), { (_: Int) + 1 }
      )

    "enumerate enumerators of correct type" in {
      member(0) shouldBe a [Member[_]]
    }

  }
}