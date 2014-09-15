package scife
package enumeration

import org.scalatest._

class FiniteTest extends FunSuite with Matchers {

  test("simple gets") {
    val stream = WrapArray(Vector(5, 2, 3))
    		
    stream.size should be (3)
    
    (0 until 3).map(stream(_)) should be (
      List(5, 2, 3)
    )
    
    stream(2) should be (3)
    stream(2) should be (3)
  }
  
  test("out of range") {
    val stream = WrapArray(Vector(1, 2, 3).zipWithIndex)
    
    stream.size should be (3)
    
    intercept[IndexOutOfBoundsException](stream(3))
  }
  
}
