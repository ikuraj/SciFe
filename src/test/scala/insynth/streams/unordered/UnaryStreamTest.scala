package insynth.streams.unordered

import scala.util.Random

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.Test

import insynth.streams.Streamable

class UnaryStreamTest extends JUnitSuite {    
    
  @Test
  def testUnaryStreamLazy2 {    
    {
		  lazy val streamTemp = {      
		    def loop(index: Int): Stream[Int] =
			  	1 #:: ((throw new RuntimeException) #:: Stream.empty[Int])
					  
			  // start with first iterator
		    loop(0)    
		  }
      
	    val streamable: Streamable[Int] = SingleStream(streamTemp, true)
	    
	    val rr = UnaryStream(streamable, { x: Int => x + 1 })
	    	    
	    val stream = rr.getStream
	    	    
	    assertTrue(rr.isInfinite)
	    	    	    
	    assertEquals(2, stream.take(1).head)
    }
  }  
  
  @Test
  def testUnaryStream = {
    
    {
	    val finiteStream = List(1,2,4).toStream
	    
	    val streamable1: Streamable[Int] = SingleStream(finiteStream, false)
	    val streamable2: Streamable[Int] = UnaryStream(streamable1, { x: Int => x + 1 })
	    
	    assertFalse(streamable2.isInfinite)
	    
	    val stream = streamable2.getStream
	    
	    assertEquals(3, stream.size)
	    assertEquals(List(2,3,5), stream.take(3).toList)
	    assertEquals(10, (0 /: stream)(_ + _))
    }
    
  }
  
}