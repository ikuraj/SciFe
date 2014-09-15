package scife.enumeration
package lzy

import org.scalatest._

class ProductInfiniteTest extends FunSuite with Matchers {

  test("simple") {
    val bs = Product(
      Enum( Stream.from(0) ),
      Enum( { (_: Int) + 1 })
    )

    bs shouldBe a [ProductInfinite[_, _]]
    bs shouldBe a [Infinite[_]]
    intercept[UnsupportedOperationException] {
      bs.size should be (9)
    }

    val res =
      (0 until 9).map(
        bs(_)
      )

    res should contain allOf ( (0, 1), (1, 3), (1, 2) )

  }

}
