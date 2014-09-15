package scife
package enumeration

import org.scalatest._

class SingletonTest extends FunSuite with Matchers {

  test("Simple") {
    val singleton = Singleton(2)

    singleton(0) should be (2)

    singleton.size should be (1)

    intercept[NoSuchElementException] {
      singleton(1)
    }

    intercept[NoSuchElementException] {
      singleton(-1)
    }
  }

}
