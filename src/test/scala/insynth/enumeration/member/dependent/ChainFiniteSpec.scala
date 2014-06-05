package insynth
package enumeration
package member
package dependent

import enumeration.{ dependent => d }

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class ChainFiniteSpec extends WordSpec with Matchers with PropertyChecks {

  val memberN = new WrapArray(
    Array( 1 to 5: _* )
  )
  val memberDep = new WrapFunctionFin(
    { (x: Int) =>
      new WrapArray(Array(1 to x: _*))
    })

  val member = new ChainFinite(
    memberN, memberDep
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

  "A ChainFinite Member" when {
    "simple case" should {

      "have appropriate size" in {
        member.size should be (5 + 4 + 3 + 2 + 1)
      }

      "enumerate correctly" in {
        member(0) should be ( (1, 1) )
        member.toList.take(6).map(_._2) should be ( List(1, 1, 2, 1, 2, 3) )
      }
  
      val gen = Gen.choose(1, 20)
      
      "member correctly" in {
        for ( ind <- 0 until normal.size) {
          member.member(normal(ind)) should be (true)
        }
      }

    }

  }
}