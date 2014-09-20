package insynth
package enumeration
package reverse
package dependent

import enumeration.{ dependent => d }

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class ProductFiniteSpec extends WordSpec with Matchers with PropertyChecks {

  val reverseDep = new WrapFunctionFin(
    { (x: Int) =>
      new WrapArray(Array(1 to x: _*))
    })

  val reverse = new ProductFinite(
    reverseDep, reverseDep
  )

  val normalDep = new d.WrapFunctionFin(
    { (x: Int) =>
      new WrapArray(Array(1 to x: _*))
    })
 
  val normal = new d.ProductFinite(
    normalDep, normalDep
  )

  "A ProductFinite Reverse" when {
    "simple case" should {

      "have appropriate size" in {
        reverse(0).size should be (0)
        reverse(1).size should be (1)
        reverse(2).size should be (4)
        reverse(5).size should be (25)
      }

      "enumerate correctly" in {
        reverse(10)(0) should be ( (1, 1) )
        reverse(9).toList should be ( normal(9).toList )
      }
  
      val gen = Gen.choose(1, 20)
      
      "reverse correctly" in {
        forAll(gen, minSuccessful(5)) { (dp: Int) =>
          reverse(dp).size should be (normal(dp).size)

          forAll(Gen.choose(0, dp * dp), minSuccessful(5)) { (ind: Int) =>
            whenever (ind < dp *dp) {
              for ( ind <- 0 until normal(dp).size) {
                reverse(dp).reverse(normal(dp)(ind)) should be (ind)
              }
            }
          }
        }
      }

    }

  }
}