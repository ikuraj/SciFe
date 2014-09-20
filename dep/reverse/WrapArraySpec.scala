package insynth
package enumeration
package reverse

import org.scalatest._

class WrapArraySpec extends WordSpec with Matchers {
  
  val reverse = new WrapArray( Array(1 to 10: _*) )

  "A WrapArray Reverse" should {
    "have appropriate size" in {
      assert(reverse.size == 10)
    }
    
    "have definite size" in {
      reverse.hasDefiniteSize should be (true)
    }

    "enumerate correctly" in {
      reverse(0) should be (1)
      reverse(9) should be (10)
    }

    "reverse correctly" in {
      reverse.reverse(10) should be (9)
      reverse.reverse(1) should be (0)
    }

    "produce exception when reverse non-existing" in {
      intercept[NoSuchElementException] {
        reverse.reverse(0)
      }
      intercept[NoSuchElementException] {
        reverse.reverse(15)
      }
    }
  }
}