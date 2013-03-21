package insynth.streams.unordered

import scala.util.Random

import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen

import insynth.streams.Streamable

class SingletonTest extends FunSpec with GivenWhenThen {    
  
  describe("A Singleton") {
    
    it("should return only one Given value") {
      
      Given("a Singleton")
      val randomInt = new Random(System.currentTimeMillis()).nextInt
      val streamable: Streamable[Int] = Singleton(randomInt)
      
      Then("it should not be infinite")
      assert(!streamable.isInfinite)
      
	    val stream = streamable.getStream
      And("the head of its stream should be the Given value")	    
	    expectResult(randomInt) {
        stream.head
      } 
	    
      And("its tail should be empty")     
	    expectResult(Nil) { stream.tail }
    }
    
  }
  
}