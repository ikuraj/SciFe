package insynth
package enumeration

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class SingletonTest extends FunSuite with ShouldMatchers {    
  
  test("Simple") {
    val singleton = Singleton(2)
    
    singleton(0) should be (2)
    
    singleton.size should be (1)
   
		intercept[NoSuchElementException] {
      singleton(1)
		}
    
		intercept[NoSuchElementException] {
      singleton(-1)
		}
  }
    
}