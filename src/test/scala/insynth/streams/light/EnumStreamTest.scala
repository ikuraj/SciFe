package insynth
package streams.light

import org.scalatest._

import util._
import common._
import util.format._

class EnumStreamTest extends FunSuite with ShouldMatchers {

  test("simple stream, infinite") {
    val enum = Enum(Stream.from(1))
    		
    enum.hasDefiniteSize should be (false)
    enum.size should be (-1)
    
    val stream = EnumStream(enum)
        
    stream.hasDefiniteSize should be (false)
//    stream.size should be (-1)
    stream.take(5) should be (Stream.range(1, 6))
  }
  
  test("simple stream, finite") {
    val innerStream = List(5, 4, 3, 2, 1)
    val enum = Enum(innerStream)

    innerStream.hasDefiniteSize should be (true)
    enum.hasDefiniteSize should be (true)
    enum.size should be (5)    
    
    val stream = EnumStream(enum)
        
    stream.hasDefiniteSize should be (false)
    stream.size should be (5)
    stream.take(5) should be (Stream(5, 4, 3, 2, 1))
  }
  
}
