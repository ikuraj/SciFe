package insynth
package enumeration
package member

import insynth.{ enumeration => e }

import org.scalatest._

class ProductFiniteSpec extends WordSpec with Matchers {
  
  val member = new ProductFinite( 
    new WrapArray( Array(1 to 10: _*) ),
    new WrapArray( Array(10 to 1 by -1: _*) )
  )

  val normal = new e.lzy.ProductFinite( 
    new e.WrapArray( Array(1 to 10: _*) ),
    new e.WrapArray( Array(10 to 1 by -1: _*) )
  )

  "A finite Product Member" should {
    "have appropriate size" in {
      assert(member.size == 10 * 10)
    }
    
    "have definite size" in {
      member.hasDefiniteSize should be (true)
    }

    "enumerate correctly" in {
      member(0) should be ( (1, 10) )
      member(9) should be ( normal(9) )
    }

    "member correctly" in {
      for ( ind <- 0 until normal.size) {
        member.member(normal(ind)) shouldBe true
      }

      member.member( (11, 0) )
    }

  }
}