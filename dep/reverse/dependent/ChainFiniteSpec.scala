package insynth
package enumeration
package reverse
package dependent

import enumeration.{ dependent => d }

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class ChainFiniteSpec extends WordSpec with Matchers with PropertyChecks {

  val reverseN = new WrapArray(
    Array( 1 to 5: _* )
  )
  val reverseDep = new WrapFunctionFin(
    { (x: Int) =>
      new WrapArray(Array(1 to x: _*))
    })

  val reverse = new ChainFinite(
    reverseN, reverseDep
  )

  val normalN = new WrapArray(
    Array( 1 to 5: _* )
  )
  val normalDep = new d.WrapFunctionFin(
    { (x: Int) =>
      new WrapArray(Array(1 to x: _*))
    })
 
  val normal = new d.ChainFinite(
    normalN, normalDep
  )

  "A ChainFinite Reverse" when {
    "simple case" should {

      "have appropriate size" in {
        reverse.size should be (5 + 4 + 3 + 2 + 1)
      }

      "enumerate correctly" in {
        reverse(0) should be ( (1, 1) )
        reverse.toList.take(6).map(_._2) should be ( List(1, 1, 2, 1, 2, 3) )
      }
  
      val gen = Gen.choose(1, 20)
      
      "reverse correctly" in {
        for ( ind <- 0 until normal.size) {
          reverse.reverse(normal(ind)) should be (ind)
        }
      }

    }

  }
}