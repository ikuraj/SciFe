package insynth
package enumeration
package reverse

import org.scalatest._

class EmptySpec extends WordSpec with Matchers {
  
  val emptyReverse = new Empty[Int]

  "An Empty Reverse" should {
    "have size 0" in {
      assert(emptyReverse.size == 0)
    }
    
    "have definite size" in {
      emptyReverse.hasDefiniteSize should be (true)
    }

    "produce exception when enumerate" in {
      intercept[NoSuchElementException] {
        emptyReverse(0)
      }
    }

    "produce exception when reverse" in {
      intercept[NoSuchElementException] {
        emptyReverse.reverse(0)
      }
      intercept[NoSuchElementException] {
        emptyReverse.reverse(5)
      }
    }
  }
}