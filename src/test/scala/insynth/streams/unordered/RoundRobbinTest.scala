package insynth.streams.unordered

import scala.util.Random

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.Test

import insynth.streams.Streamable

class RoundRobbinTest extends JUnitSuite {    
  
  def printStream[A](stream: Stream[A])(n: Int) = stream.take(n).toList mkString (", ")
  
  @Test
  def testRoundRobbin {    
    {
	    val finiteStream1 = List(1,2,3).toStream
	    val finiteStream2 = List(4,5,6).toStream
	    
	    val streamable1: Streamable[Int] = SingleStream(finiteStream1, false)
	    val streamable2: Streamable[Int] = SingleStream(finiteStream2, false)
	    
	    val rr = RoundRobbin(Array(streamable1, streamable2))
	    	    
	    val stream = rr.getStream
	    	    
	    assertFalse(rr.isInfinite)
	    
	    assertEquals(6, stream.size)
	    	    
	    assertEquals(List(1, 4, 2, 5), stream.take(4))
    }
        
    {
      val rnd = new Random(System.currentTimeMillis())
    	val randomInt1 = rnd.nextInt    	
			val randomInt2 = rnd.nextInt
    	
	    val infiniteStream1 = Stream.continually(randomInt1)
	    val infiniteStream2 = Stream.continually(randomInt2)
	    
	    val streamable1: Streamable[Int] = SingleStream(infiniteStream1, true)
	    val streamable2: Streamable[Int] = SingleStream(infiniteStream2, true)
	    
	    val rr = RoundRobbin(Array(streamable1, streamable2))
	    	    
	    val stream = rr.getStream
	    	    
	    assertTrue(rr.isInfinite)
	    
	    assertEquals(100, stream.take(100).size)
	    	    
	    assertEquals(List(randomInt1, randomInt2, randomInt1, randomInt2), stream.take(4))
    }    
    
    {
      val rnd = new Random(System.currentTimeMillis())
    	val randomInt1 = rnd.nextInt
    	
	    val infiniteStream1 = Stream.continually(randomInt1)
	    val finiteStream2 = List(4,5,6).toStream
	    
	    val streamable1: Streamable[Int] = SingleStream(infiniteStream1, true)
	    val streamable2: Streamable[Int] = SingleStream(List(4,5,6).toStream, false)
	    
	    val rr = RoundRobbin(Array(streamable1, streamable2))
	    	    
	    val stream = rr.getStream
	    	    
	    assertTrue(rr.isInfinite)
	    
	    assertEquals(100, stream.take(100).size)
	    
	    val compareList = List(randomInt1, 4, randomInt1, 5, randomInt1, 6, randomInt1, randomInt1, randomInt1)
	    	    
	    assertEquals(compareList, stream.take(compareList.size))
    }
  }
  
  @Test
  def testRoundRobbin2 {    
    {
	    val finiteStream1 = List(1).toStream
	    val finiteStream2 = List(2).toStream
	    val finiteStream3 = List(3).toStream
	    
	    val streamable1: Streamable[Int] = SingleStream(finiteStream1, false)
	    val streamable2: Streamable[Int] = SingleStream(finiteStream2, false)
	    val streamable3: Streamable[Int] = SingleStream(finiteStream3, false)
	    
	    val rr = RoundRobbin(Array(streamable1, streamable2, streamable3))
	    	    
	    val stream = rr.getStream
	    	    
	    assertFalse(rr.isInfinite)
	    
	    assertEquals(3, stream.size)
	    	    
	    assertEquals(List(1, 2, 3), stream.take(3))
    }
  }
  
  @Test
  def testRoundRobbin3 {    
    {
	    val streamable1: Streamable[Int] = Singleton(1)
	    val streamable2: Streamable[Int] = Singleton(2)
	    val streamable3: Streamable[Int] = Singleton(3)
	    
	    val rr = RoundRobbin(Array(streamable1, streamable2, streamable3))
	    	    
	    val stream = rr.getStream
	    	    
	    assertFalse(rr.isInfinite)
	    
	    assertEquals(3, stream.size)
	    	    
	    assertEquals(List(1, 2, 3), stream.take(3))
    }
  }
  
  @Test
  def testRoundRobbin4 {    
    {
	    val streamable1: Streamable[Int] = Singleton(1)
	    val streamable2: Streamable[Int] = Singleton(2)
	    
	    val rr = RoundRobbin(Array(streamable1, streamable2))
	    	    
	    val stream = rr.getStream
	    	    
	    assertFalse(rr.isInfinite)
	    	    
	    assertEquals(List(1), stream.take(1))
	    assertEquals(1, stream.iterator.next)
    }
  }
  
}