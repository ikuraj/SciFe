package scife
package enumeration
package member

import org.scalatest._

class WrapArraySpec extends WordSpec with Matchers {
  
  val member = new WrapArray( Array(1 to 10: _*) )

  "A WrapArray Member" should {
    "have appropriate size" in {
      assert(member.size == 10)
    }
    
    "have definite size" in {
      member.hasDefiniteSize should be (true)
    }

    "enumerate correctly" in {
      member(0) should be (1)
      member(9) should be (10)
    }

    "member correctly" in {
      member.member(10) shouldBe true
      member.member(1) shouldBe true
      member.member(15) shouldBe false
    }
  }
}