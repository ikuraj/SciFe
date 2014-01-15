package insynth.streams
package unordered

import scala.util.Random

import org.scalatest.FunSuite

import insynth.streams.Streamable

class SingleStreamTest extends FunSuite {
  
  import Utils._
  
  def printStream[A](stream: Stream[A])(n: Int) = stream.take(n).toList mkString (", ")
  
  test("infinite stream test") {
    val randomInt = new Random(System.currentTimeMillis()).nextInt
  
    val infiniteStream = Stream.continually(randomInt)
    
    val streamable: Streamable[Int] = SingleStream(infiniteStream, true)
    
    assert(streamable.isInfinite)
    
    val stream = streamable.getStream
    
    expectResult(100) { stream.take(100).size }
      
    compareCallsToGetStream( List(streamable) )
  }
    
  test("finite stream test")  {
    
    val finiteStream = List(1,2,3).toStream
    
    val streamable: Streamable[Int] = SingleStream(finiteStream, false)
    
    assert(!streamable.isInfinite)
    
    val stream = streamable.getStream
    
    expectResult(6) { (0 /: stream)(_ + _) }
      
    compareCallsToGetStream( List(streamable) )
  }
  
}