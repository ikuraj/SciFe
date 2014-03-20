package insynth
package streams.light

import org.scalatest._

import util._
import common._
import util.format._

class EnumTest extends FunSuite with ShouldMatchers {

  test("factory method") {
    
    {
	    val enum = Enum((Set(1, 2, 3) map { Set(_) }).toList)
	    		
	    enum shouldBe a [WrapperArray[_]]
	    enum.hasDefiniteSize should be (true)
	    enum.size should be (3)
    }
    {
	    val enum = Enum(Set(1, 2, 3))
	    		
	    enum shouldBe a [WrapperArray[_]]
	    enum.hasDefiniteSize should be (true)
	    enum.size should be (3)
    }
    
  }
  
}
