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
    
    stream.size should be (3)
    stream.isInfinite should be (false)
    
    val it = stream.getStream.iterator
    it.next
    
    stream.getStream.toList should be (
      Vector(1, 2, 3)
    )

    it.next

    stream.getStream.toList should be (
      Vector(1, 2, 3)
    )

    it.next

    stream.getValuedStream(2) should be ( (3, 2) )
    
    it.hasNext should be (false)
  }
  
}
