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
      // set is treated as a function !
	    val enum = Enum[Set[Int]]( Set(1, 2, 3) )
	    		
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

  test("factory method, function") {
    
    {
      val a = Predef.identity[Int] _
      val b = Predef.identity[Int] _
      
      assert( a != b )
    }
    {
	    val enum = Enum[Int]( Predef.identity[Int] _ )
	    		
	    enum shouldBe a [WrapFunction[_]]
	    enum.hasDefiniteSize should be (false)
    }
    {
      // set is treated as a function !
	    val enum = Enum( Set(1, 2, 3) )
	    		
	    enum shouldBe a [WrapFunction[_]]
	    enum(3) should be (true)
	    enum(4) should be (false)
	    enum.hasDefiniteSize should be (false)
    }
    
  }
  
}
