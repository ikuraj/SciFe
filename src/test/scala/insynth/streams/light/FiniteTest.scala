package insynth
package streams.light

import org.scalatest._
import org.scalatest.matchers._

import util._
import common._
import util.format._

class FiniteTest extends FunSuite with ShouldMatchers {

  test("simple gets") {
    val stream = WrapperArray(Vector(5, 2, 3))
    		
    stream.size should be (3)
    
    (0 until 3).map(stream(_)) should be (
      List(5, 2, 3)
    )
    
    stream(2) should be (3)
    stream(2) should be (3)
  }
  
  test("out of range") {
    val stream = WrapperArray(Vector(1, 2, 3).zipWithIndex)
    
    stream.size should be (3)
    
    intercept[IndexOutOfBoundsException](stream(3))
  }
  
}
