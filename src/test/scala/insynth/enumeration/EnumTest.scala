package insynth
package enumeration

import org.scalatest._

class EnumTest extends FunSuite with Matchers {

  test("factory method") {
    
    {
	    val enum = Enum((Set(1, 2, 3) map { Set(_) }).toList)
	    		
	    enum shouldBe a [WrapArray[_]]
	    enum.hasDefiniteSize should be (true)
	    enum.size should be (3)
    }
    {
	    val enum = Enum(Set(1, 2, 3))
	    		
	    enum shouldBe a [Singleton[_]]
	    enum.hasDefiniteSize should be (true)
	    enum.size should be (1)
    }
    {
      val enum = Enum(1, 2, 3)
          
      enum shouldBe a [WrapArray[_]]
      enum.hasDefiniteSize should be (true)
      enum.size should be (3)
    }
    
  }
  
}
