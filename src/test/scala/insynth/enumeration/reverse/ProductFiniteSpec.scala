package insynth
package enumeration
package reverse

import insynth.{ enumeration => e }

import org.scalatest._

class ProductFiniteSpec extends WordSpec with Matchers {
  
  val reverse = new ProductFinite( 
    new WrapArray( Array(1 to 10: _*) ),
    new WrapArray( Array(10 to 1 by -1: _*) )
  )

  val normal = new e.lzy.ProductFinite( 
    new e.WrapArray( Array(1 to 10: _*) ),
    new e.WrapArray( Array(10 to 1 by -1: _*) )
  )

  "A finite Product Reverse" should {
    "have appropriate size" in {
      assert(reverse.size == 10 * 10)
    }
    
    "have definite size" in {
      reverse.hasDefiniteSize should be (true)
    }

    "enumerate correctly" in {
      reverse(0) should be ( (1, 10) )
      reverse(9) should be ( normal(9) )
    }

    "reverse correctly" in {
      val reverseMap =
        normal.zipWithIndex.map( { case (x, y) => (y, x) } ).toMap
      
      for ( ind <- 0 until normal.size) {
        reverse.reverse(normal(ind)) should be (ind)
      }
    }

    "produce exception when reverse non-existing" in {
      intercept[NoSuchElementException] {
        reverse.reverse( (11, 0) )
      }
    }
  }
}