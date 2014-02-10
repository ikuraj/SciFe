package insynth.streams
package light

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class BinaryTest extends FunSuite with ShouldMatchers {    
  
  test("simple") {
    val bs = Binary(
      WrapperArray(1, 2, 3),
      WrapperArray(4, 5, 6)
    )((_, _))
    
    val res =
		  (0 until 9).map(
		    bs(_)
		  )
		  
	  res should contain allOf ( (1, 4), (2, 5), (1, 6), (3, 6) )
    
  }

}