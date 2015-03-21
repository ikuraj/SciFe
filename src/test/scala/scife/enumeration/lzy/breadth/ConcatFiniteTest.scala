package scife.enumeration
package lzy
package breadth

import org.scalatest._

class ConcatFiniteTest extends FunSuite with Matchers {

  test("Simple accesses, ConcatFiniteVariedSize") {
    val arrays: Array[Finite[Int]] = Array(
      WrapArray(1, 2, 3),
      WrapArray(4, 5, 6),
      WrapArray(7, 8, 9, 10, 11),
      WrapArray(7, 8, 9, 10, 11, 12) map { _ + 5 },
      WrapArray(7, 8, 9, 10, 11, 12) map { _ + 11 }
    )

    val rr = new ConcatFiniteVariedSize(arrays)
    assert(rr.size == 23)

    for (ind <- 0 until 23) {
      rr(ind) == ind
    }

  }

}
