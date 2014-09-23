package scife
package enumeration
package member
package dependent

import org.scalatest._

class MemberDependSpec extends WordSpec with Matchers {

  val member = new WrapFunction(
    { (x: Int) =>
      new WrapArray(Array(1 to x: _*))
    })

  "A depend Member" should {

    "enumerate enumerators with appropriate type" in {
      member(0) shouldBe a [Member[Int]]
    }

  }

}