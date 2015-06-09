package scife
package enumeration
package member

import scife.{ enumeration => e }

import org.scalatest._

class MapSpec extends WordSpec with Matchers {

  def swap(p: (Int, Int)) = {
    val (v1, v2) = p
    (v2, v1)
  }

  val normal = new e.lzy.ProductFinite(
    new e.WrapArray(Array(1 to 10: _*)),
    new e.WrapArray(Array(10 to 1 by -1: _*))) map swap

  val member =
    new Map(
      new ProductFinite(
        new WrapArray(Array(1 to 10: _*)),
        new WrapArray(Array(10 to 1 by -1: _*))),
      swap,
      swap) with MemberFinite[(Int, Int)]

  "A finite Map Member" when {
    "built as product and membership of pairs" should {
      "have appropriate size" in {
        assert(member.size == normal.size)
      }

      "have definite size" in {
        member.hasDefiniteSize should be(true)
      }

      "enumerate correctly" in {
        member(0) should be((10, 1))
        member(9) should be(normal(9))
      }

      "test membership correctly" in {
        member.member( (0, 0) ) shouldBe false
        member.member( (1, 9) ) shouldBe true
      }
    }
  }
}