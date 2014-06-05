package insynth
package enumeration
package member
package dependent

import enumeration.{ dependent => d }

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class ProductFiniteSpec extends WordSpec with Matchers with PropertyChecks {

  val memberDep = new WrapFunctionFin(
    { (x: Int) =>
      new WrapArray(Array(1 to x: _*))
    })

  val member = new ProductFinite(
    memberDep, memberDep
  )

  val normalDep = new d.WrapFunctionFin(
    { (x: Int) =>
      new WrapArray(Array(1 to x: _*))
    })
 
  val normal = new d.ProductFinite(
    normalDep, normalDep
  )

  "A ProductFinite Member" when {
    "simple case" should {

      "have appropriate size" in {
        member(0).size should be (0)
        member(1).size should be (1)
        member(2).size should be (4)
        member(5).size should be (25)
      }

      "enumerate correctly" in {
        member(10)(0) should be ( (1, 1) )
        member(9).toList should be ( normal(9).toList )
      }
  
      val gen = Gen.choose(1, 20)
      
      "member correctly" in {
        forAll(gen, minSuccessful(5)) { (dp: Int) =>
          member(dp).size should be (normal(dp).size)

          forAll(Gen.choose(0, dp * dp), minSuccessful(5)) { (ind: Int) =>
            whenever (ind < dp *dp) {
              for ( ind <- 0 until normal(dp).size) {
                member(dp).member(normal(dp)(ind)) should be (true)
              }
            }
          }
        }
      }

    }

  }
}