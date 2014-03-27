package insynth
package enumeration

import util.EnumStream

import org.scalatest._

class EnumStreamTest extends FunSuite with Matchers {

  test("simple stream, infinite") {
    val enum = Enum(Stream.from(1))
    		
    enum.hasDefiniteSize should be (false)
    
    val stream = EnumStream(enum)
        
    stream.hasDefiniteSize should be (false)
    stream.take(5) should be (Stream.range(1, 6))
  }
  
  test("simple stream, finite") {
    val innerStream = List(5, 4, 3, 2, 1)
    val enum = Enum(innerStream)

    innerStream.hasDefiniteSize should be (true)
    enum shouldBe a [WrapArray[_]]
    enum.hasDefiniteSize should be (true)
    enum.size should be (5)    
    
    intercept[IllegalArgumentException] {
      EnumStream(enum)
    }
        
  }
  
}
