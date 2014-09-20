package scife.enumeration
package lzy

import scife.util.RunnableJUnit
import org.scalatest._

class ConcatFiniteTest extends FunSuite with Matchers with RunnableJUnit {

  test("Simple accesses, equal") {
    val rr = ConcatFinite.equal(Array[Finite[Int]](
      WrapArray(1, 2, 3),
      WrapArray(4, 5, 6),
      WrapArray(7, 8, 9)
    ))

    (0 until 9).map(
      rr(_)
    ) should be ( List(1, 4, 7, 2, 5, 8, 3, 6, 9) )

  }

  test("Simple accesses, fixed") {
    val arrays: Array[Finite[Int]] = Array(
      WrapArray(1, 2, 3),
      WrapArray(4, 5, 6),
      WrapArray(7, 8, 9)
    )
    arrays.size should be (3)

    val rr = ConcatFinite.fixed[Int](arrays)

    for (target <- 0 until 9) {
      rr.binarySearch(target) should be (target / 3)
    }

    (0 until 9).map(
      rr.apply(_)
    ) should be ( 1 to 9 )

  }

  test("Simple accesses, fixed, one-element Enums") {
    val arrays: Array[Finite[Int]] = Array(
      WrapArray(1),
      WrapArray(4),
      WrapArray(7)
    )
    arrays.size should be (3)

    val rr = ConcatFinite.fixed[Int](arrays)

    rr.size should be (3)
    for (target <- 0 until 3) {
      rr.binarySearch(target) should be (target)
    }

//    (0 until 3).map(
//      rr.apply(_)
//    ) should be ( List(1, 4, 7) )

  }

  test("Simple accesses and appending, buffer") {
    val rr = ConcatFinite.buffer(Array(
      WrapArray(1 to 30),
      WrapArray(31 to 60),
      WrapArray(61 to 90)
    ))

    for (target <- 0 to 89) {
      rr.binarySearch(target) should be (target / 30)
      rr(target) should be (target + 1)
    }

    rr.append(WrapArray(91 to 120))

    for (target <- 0 to 119) {
      rr.binarySearch(target) should be (target / 30)
      rr(target) should be (target + 1)
    }

  }

  test("RoundRobbin with empty array") {
    val rr = ConcatFinite.fixed[Int](Array(
    ))

    rr.size should be (0)
  }

}
