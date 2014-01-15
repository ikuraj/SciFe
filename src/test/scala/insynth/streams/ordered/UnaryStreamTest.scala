package insynth.streams.ordered

import scala.util.Random

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.Test

class UnaryStreamTest extends JUnitSuite {
  
  import Utils.getSingleStream
    
  @Test
  def testUnaryStream = {
    
    {
	    val finiteStream = List(1,2,4).toStream
	    
	    val streamable1 = getSingleStream(finiteStream, false)
	    val streamable2 = UnaryStream(streamable1, { x: Int => x + 1 })
	    
	    assertFalse(streamable2.isInfinite)
	    
	    val stream = streamable2.getStream
	    
//	    assertTrue(stream.hasDefiniteSize)
	    assertEquals(3, stream.size)
	    assertEquals(List(2,3,5), stream.take(3).toList)
	    assertEquals(10, (0 /: stream)(_ + _))
    }
    
  }
  
  @Test
  def testModifyValues = {
    
    {
	    val finiteStream = List(1,2,4).toStream
	    
	    val streamable1 = getSingleStream(finiteStream, false)
	    val streamable2 = UnaryStream(streamable1, { x: Int => x + 1 }, identity)
	    
	    assertFalse(streamable2.isInfinite)
	    
	    val stream = streamable2.getStream
	    
	    assertEquals(3, stream.size)
	    assertEquals(List(2,3,5), stream.take(3).toList)
	    assertEquals(10, (0 /: stream)(_ + _))
	    
	    val valueStream = streamable2.getValues
	    
	    assertEquals(3, valueStream.size)
	    assertEquals(List(1,2,4), valueStream.take(3).toList)
	    assertEquals(7, (0 /: valueStream)(_ + _))
    }
    
    {
	    val finiteStream = List(1,2,4).toStream
	    
	    val streamable1 = getSingleStream(finiteStream, false)
	    val streamable2 = UnaryStream(streamable1, { x: Int => x + 1 }, _ + 1)
	    
	    assertFalse(streamable2.isInfinite)
	    
	    val stream = streamable2.getStream
	    
	    assertEquals(3, stream.size)
	    assertEquals(List(2,3,5), stream.take(3).toList)
	    assertEquals(10, (0 /: stream)(_ + _))
	    
	    val valueStream = streamable2.getValues
	    
	    assertEquals(3, valueStream.size)
	    assertEquals(List(2,3,5), valueStream.take(3).toList)
	    assertEquals(10, (0 /: valueStream)(_ + _))
    }
    
  }
  
}