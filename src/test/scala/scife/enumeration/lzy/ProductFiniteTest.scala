package scife.enumeration
package lzy

import org.scalatest._

class ProductFiniteTest extends FunSuite with Matchers {

  test("simple") {
    val bs = Product(
      WrapArray(1, 2, 3),
      WrapArray(4, 5, 6)
    )

    bs.size should be (9)

    val res =
      (0 until 9).map(
        bs(_)
      )

    res should contain allOf ( (1, 4), (2, 5), (1, 6), (3, 6) )

  }

}
