package insynth.streams.unordered

import scala.util.Random

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.Test

class ScalaStreamTest extends JUnitSuite {    
  
  def printStream[A](stream: Stream[A])(n: Int) = stream.take(n).toList mkString (", ")
  
  @Test
  def testStreamLoop {
    
	  val stream = {      
	    def loop(index: Int): Stream[Int] =
		  	1 #:: ((throw new RuntimeException) #:: Stream.empty[Int])
				  
		  // start with first iterator
	    loop(0)    
	  }
	  
    assertEquals(1, stream.take(1).head) 
    
	  val stream2 = stream map { _ + 1 }
    
    assertEquals(2, stream2.take(1).head)    
  }
  
  
  @Test
  def testUnaryStreamLazy1 {    
    {
		  lazy val streamTemp = {      
		    def loop(index: Int): Stream[Int] =
			  	1 #:: ((throw new RuntimeException) #:: Stream.empty[Int])
					  
			  // start with first iterator
		    loop(0)    
		  }
	    	    	    
	    assertEquals(1, streamTemp.take(1).head)
    }
  }
  
}