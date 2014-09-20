package insynth
package enumeration
package reverse

import insynth.{ enumeration => e }

import org.scalatest._

class MapSpec extends WordSpec with Matchers {

  def swap(p: (Int, Int)) = {
    val (v1, v2) = p
    (v2, v1)
  }

  val normal = new e.lzy.ProductFinite(
    new e.WrapArray(Array(1 to 10: _*)),
    new e.WrapArray(Array(10 to 1 by -1: _*))) map swap

  val reverse =
    new Map(
      new ProductFinite(
        new WrapArray(Array(1 to 10: _*)),
        new WrapArray(Array(10 to 1 by -1: _*))),
      swap,
      swap) with ReverseFinite[(Int, Int)]

  "A finite Map Reverse" when {
    "built as product and reverse of pairs" should {
      "have appropriate size" in {
        assert(reverse.size == normal.size)
      }

      "have definite size" in {
        reverse.hasDefiniteSize should be(true)
      }

      "enumerate correctly" in {
        reverse(0) should be((10, 1))
        reverse(9) should be(normal(9))
      }

      "reverse correctly" in {
        val reverseMap =
          normal.zipWithIndex.map({ case (x, y) => (y, x) }).toMap

        for (ind <- 0 until normal.size) {
          reverse.reverse(normal(ind)) should be(ind)
        }
      }

      "produce exception when reverse non-existing" in {
        intercept[NoSuchElementException] {
          reverse.reverse((11, 0))
        }
      }
    }
  }
}