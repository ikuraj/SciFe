package insynth
package streams.ordered

import org.scalatest._
import org.scalatest.matchers._

import reconstruction.stream.Application
import reconstruction.stream._

import util._
import common._
import util.format._

class CountedTest extends FunSuite with ShouldMatchers {

  test("finite streamable test") {
    val streamable = new FiniteStream(Vector(1, 2, 3).zipWithIndex) with OrderedCountable[Int]

    streamable.enumerated should be (0)
    val stream = streamable.getValuedStream
    streamable.enumerated should be (1)
    
    stream.head    
    streamable.enumerated should be (1)
    stream(1)    
    streamable.enumerated should be (2)
    stream(2)    
    streamable.enumerated should be (3)
    stream(2)    
    streamable.enumerated should be (3)
  }

  test("infinite streamable test") {
    val streamable = new WrapperStream(Stream.from(1).zipWithIndex) with OrderedCountable[Int]
    val stream = streamable.getValuedStream
    
    streamable.enumerated should be (1)
    
    for (toTake <- 100 to 1000 by 100) {
      // force evaluate
      stream.take(toTake).toList    
      streamable.enumerated should be (toTake)
    }
  }
  
}
