package insynth
package streams.ordered

import org.scalatest._
import org.scalatest.matchers._

import reconstruction.stream.Application
import reconstruction.stream._

import util._
import common._
import util.format._

class FiniteStreamTest extends FunSuite with ShouldMatchers {

  test("simple enumeration test") {
    val stream = FiniteStream(Vector(1, 2, 3).zipWithIndex)
    
    stream.getStream.toList should be (
      Vector(1, 2, 3)
    )
  }
  
  test("streamable test") {
    val stream = FiniteStream(Vector(1, 2, 3).zipWithIndex)
    
    stream.nextReady(0) should be (true)
    stream.isDepleted should be (false)
    
    val it = stream.getStream.iterator
    stream.nextReady(0) should be (true)
    stream.isDepleted should be (false)
    stream.nextReady(0) should be (true)
    stream.isDepleted should be (false)
    
    it.next
    
    stream.nextReady(1) should be (true)
    stream.isDepleted should be (false)
    stream.nextReady(1) should be (true)
    stream.isDepleted should be (false)
    
    it.next
    
    stream.nextReady(2) should be (true)
    stream.isDepleted should be (false)
    stream.nextReady(2) should be (true)
    stream.isDepleted should be (false)
    
    it.next
    
    stream.nextReady(3) should be (false)
    stream.isDepleted should be (true)
    stream.nextReady(3) should be (false)
    stream.isDepleted should be (true)
    
    stream.getStream.toList should be (
      Vector(1, 2, 3)
    )

    stream.getStream.toList should be (
      Vector(1, 2, 3)
    )
  }
  
}
