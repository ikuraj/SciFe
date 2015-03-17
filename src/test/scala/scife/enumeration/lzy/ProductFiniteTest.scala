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

  test("product of list of finites") {
    val bs = Product(
      Array[Finite[Int]](
        WrapArray(1, 2, 3),
        WrapArray(4, 5, 6),
        WrapArray(7, 8, 9)
      )
    )

    bs shouldBe a [ProductFiniteList[_]]
    bs.size should be (27)

    val res =
      (0 until bs.size).map(
        bs(_)
      )

    res should contain allOf ( List(1, 4, 8), List(2, 5, 9), List(1, 6, 7), List(3, 6, 8),
      List(1, 4, 7), List(3, 6, 9) )

  }

}
