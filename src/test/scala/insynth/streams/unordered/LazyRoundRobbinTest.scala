package insynth.streams
package unordered

import scala.util.Random

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.Test

import insynth.streams.Streamable

class LazyRoundRobbinTest extends JUnitSuite {
  
  import Utils._
  
  val rnd = new Random(System.currentTimeMillis())

  @Test
  def testTwoElementCaseLoop {    
    val stream1 = SingleStream(Stream(1, 2), false)
    
    val rr = LazyRoundRobbin(List(stream1))
    
    rr.streams :+= rr
    
    rr.initialize
    
    val stream = rr.getStream
    
    lazy val streamMessage = "Stream contents: " + stream.take(20).mkString(", ") 
    
    assertEquals(streamMessage, 1, stream.head)
    assertEquals(streamMessage, 1, stream.tail.head)
    assertEquals(streamMessage, 2, stream.tail.tail.head)
    assertEquals(streamMessage, 1, stream.tail.tail.tail.head)
    assertEquals(streamMessage, 2, stream.tail.tail.tail.tail.head)
    
    for (ind <- 1 to rnd.nextInt(1000)) {
      assertEquals(streamMessage, 2, stream(ind*2))
      assertEquals(streamMessage, 1, stream(ind*2 + 1))
    }
      
    compareCallsToGetStream( List(stream1, rr) )
  }
  
  @Test(expected=classOf[RuntimeException])
  def testTwoElementCaseEvaluation {    
    val stream1: Streamable[Int] = SingleStream(1 #:: (throw new RuntimeException) #:: Stream.empty[Int], false)
    
    val rr = LazyRoundRobbin(List(stream1))
    
    rr.streams :+= rr
    
    rr.initialize
    
    val stream = rr.getStream
    
    assertEquals(1, stream.take(1).head)
    assertEquals(1, stream.head)
    
    stream(1)
    stream(2)
  }
  
  @Test
  def testLoopWithSingleton {    
    val stream1 = Singleton(1)
    
    val rr = LazyRoundRobbin(List(stream1))
    
    rr.streams :+= rr
    
    rr.initialize
    
    val stream = rr.getStream
    
    assertEquals(1, stream.take(1).head)
    assertEquals(1, stream.tail.head)
    assertEquals(1, stream.tail.tail.head)
    
    for (ind <- 1 to rnd.nextInt(1000)) {
      assertEquals(1, stream(ind))
    }
      
    compareCallsToGetStream( List(stream1, rr) )
  }
  
}