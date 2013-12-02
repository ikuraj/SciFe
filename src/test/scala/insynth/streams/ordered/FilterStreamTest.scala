package insynth
package streams.ordered

import org.scalatest._
import org.scalatest.matchers._

import reconstruction.stream.Application
import reconstruction.stream._

import util._
import common._
import util.format._

class FilterStreamTest extends FunSuite with ShouldMatchers {

  test("simple filtering test") {
    val stream = FilterStream(
      FiniteStream(Vector(1, 2, 3).zipWithIndex), { (x: Int) => x % 2 == 0 }
    )
    
    stream.getStream.toList should be (
      Vector(2)
    )
  }

  test("simple stream filtering test") {
    val stream = FilterStream(
      SingleStream(Stream(1, 2).zipWithIndex), { (x: Int) => true }
    )
    
    stream.getStream.head should be (
      1
    )
  }

  test("simple Scala stream with exception filtering test") {
    {
      val stream = (1 #:: (throw new RuntimeException) #:: Stream[Int]()) filter
        { (x: Int) => true }
          
      stream.head should be (1)
    }
    {
      intercept[RuntimeException] {
      val stream = (1 #:: (throw new RuntimeException) #:: Stream[Int]()) filter
        { (x: Int) => false }
      }
    }
  }

  test("simple stream with exception filtering test") {
    val stream = FilterStream(
      SingleStream((1 #:: (throw new RuntimeException) #:: Stream[Int]()).zipWithIndex), { (x: Int) => true }
    )
    
    stream.getStream.head should be (
      1
    )
  }
  
  ignore("infinite values test") {
  test("infinite values test") {
    val stream = FilterStream(
      FiniteStream(Vector(1, 2, 3).zipWithIndex), { (x: Int) => x % 2 == 0 }
    )
    
    stream.getStream.toList should be (
      Vector(2)
    )
  }
  }

  
}
