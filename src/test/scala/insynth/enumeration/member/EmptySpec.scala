package insynth
package enumeration
package member

import org.scalatest._

class EmptySpec extends WordSpec with Matchers {
  
  val emptyMember = new Empty[Int]

  "An Empty Member" should {
    "have size 0" in {
      assert(emptyMember.size == 0)
    }
    
    "have definite size" in {
      emptyMember.hasDefiniteSize should be (true)
    }

    "produce exception when enumerate" in {
      intercept[NoSuchElementException] {
        emptyMember(0)
      }
    }

    "return false on membership" in {
      emptyMember.member(0) shouldBe false
      emptyMember.member(5) shouldBe false
    }
  }
}