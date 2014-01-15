package insynth.streams.ordered

import scala.util.Random

import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen

class SingletonTest extends FunSpec with GivenWhenThen {    
  
  describe("A Singleton") {
    
    it("should return only one given value") {
      
      Given("a Singleton")
      val randomInt = new Random(System.currentTimeMillis()).nextInt
      val streamable = Singleton(randomInt)
      
      Then("it should not be infinite")
      assert(!streamable.isInfinite)
      
	    val stream = streamable.getStream
      And("the head of its stream should be the Given value")	    
	    expectResult(randomInt) {
        stream.head
      } 
	    
      And("its tail should be empty")     
	    expectResult(Nil) { stream.tail }
      
      And("the similar should hold for its values")      
	    expectResult(1) {
        streamable.getValues.head
      } 
	    expectResult(Nil) { streamable.getValues.tail }
    }
    
    it("should return correct value") {
      
      Given("a Singleton")
      val randomInt = new Random(System.currentTimeMillis()).nextInt
      val randomInt2 = new Random(System.currentTimeMillis()).nextInt
      val streamable = Singleton(randomInt, randomInt2)
      
      Then("it should not be infinite")
      assert(!streamable.isInfinite)
      
	    val stream = streamable.getStream
      And("the head of its stream should be the Given value")	    
	    expectResult(randomInt) {
        stream.head
      } 
	    
      And("its tail should be empty")     
	    expectResult(Nil) { stream.tail }
      
      And("the similar should hold for its values")      
	    expectResult(randomInt2) {
        streamable.getValues.head
      } 
	    expectResult(Nil) { streamable.getValues.tail }
    }
    
  }
  
}