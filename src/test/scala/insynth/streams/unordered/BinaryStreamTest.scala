package insynth.streams
package unordered

import scala.util.Random

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.Test

import insynth.streams.Streamable

// enable implicit conversions
import scala.language.implicitConversions

class BinaryStreamTest extends JUnitSuite {    
  
  def printStream[A](stream: Stream[A])(n: Int) = stream.take(n).toList mkString (", ")
  
  import Utils._
   
  @Test
  def testBinaryStreamBefore {    
    val stream1 = Singleton(1)        
    val stream2 = Singleton(2)
        
    val bs = BinaryStream(stream1, stream2) {
      (x, y) => x + y
    }
    
    val stream = bs.getStream
    
    assertEquals(3, stream.take(1).head)
    
    compareCallsToGetStream( List(stream1, stream2, bs) )
  }
  
  trait Combination
  
  case class Number(number: Int) extends Combination
  
  case class NumberList(list: List[Combination]) extends Combination
  
  @Test
  def testBinaryStreamBothFinite { 
    implicit def intToNumber(number: Int) = Number(number)
    
    {
	    val finiteStream1 = List[Number](1,2,3).toStream
	    val finiteStream2 = List[Number](4,5,6).toStream
	    
	    val streamable1: Streamable[Combination] = SingleStream(finiteStream1, false)
	    val streamable2: Streamable[Number] = SingleStream(finiteStream2, false)
    
      compareCallsToGetStream( List(streamable1, streamable2) )
	    
	    {
		    val bs = BinaryStream(streamable1, streamable2) {
//		      case (x: NumberList, y: NumberList) => NumberList(x.list ++ y.list)
//		      case (x: Number, y: NumberList) => NumberList(x +: y.list)
		      case (x: NumberList, y: Number) => NumberList(x.list :+ y)	      
		      case (x: Number, y: Number) => NumberList(List(x, y))
	      }
		    
		    val leftStream = bs.leftStream
		    val leftStreamString = leftStream.toList mkString (", ")
		    
		    assertEquals(leftStreamString, 6, leftStream.size)
		    assertEquals(
	        leftStreamString,
	        List(List(1,4), List(2,4), List(2,5), List(3,4), List(3,5), List(3,6)) map
	        	{ x => NumberList(x map { Number(_) }) },
	        leftStream.toList
	      )
    
        compareCallsToGetStream( bs )
	    }
      
	    {
		    val bs = BinaryStream(streamable1, streamable2) {
		      case (x: NumberList, y: Number) => NumberList(x.list :+ y)	      
		      case (x: Number, y: Number) => NumberList(List(x, y))
	      }
		    
		    val rightStream = bs.rightStream
		    val rightStreamString = rightStream.toList mkString (", ")
    
        compareCallsToGetStream( bs )
		    
		    assertEquals(rightStreamString, 3, rightStream.size)
		    assertEquals(
	        rightStreamString,
	        List(List(1,5), List(1,6), List(2,6)) map
	        	{ x => NumberList(x map { Number(_) }) },
	        rightStream.toList
	      )
    
        compareCallsToGetStream( bs )
	    }
	    {
		    val bs = BinaryStream(streamable1, streamable2) {
		      case (x: NumberList, y: Number) => NumberList(x.list :+ y)	      
		      case (x: Number, y: Number) => NumberList(List(x, y))
	      }
		    
		    val stream = bs.getStream
		    
		    assertFalse(bs.isInfinite)	    	    
		    assertEquals(
	        stream.take(9) mkString (", "),
	        List(List(1,4), List(1,5), List(2,4), List(1,6), List(2,5), List(2,6), List(3,4), List(3,5), List(3,6)) map
	        	{ x => NumberList(x map { Number(_) }) },
	        stream.take(9).toList
	      )		    
	      		    
		    val streamString = stream.toList mkString (", ")
		    
		    assertEquals(streamString, 9, stream.size)		    	    
		    assertEquals(
	        streamString,
	        List(List(1,4), List(1,5), List(2,4), List(1,6), List(2,5), List(2,6), List(3,4), List(3,5), List(3,6)) map
	        	{ x => NumberList(x map { Number(_) }) },
	        stream.toList
	      )
    
        compareCallsToGetStream( bs )
	    }
    }
    
  }
    
  @Test
  def testBinaryStreamBothInfinite { 
    implicit def intToNumber(number: Int) = Number(number)
        
    val rnd = new Random(System.currentTimeMillis())
  	val randomInt1 = rnd.nextInt
  	val randomInt2 = rnd.nextInt
    
    {
	    val infiniteStream1: Stream[Combination] = Stream.continually(randomInt1)
	    val infiniteStream2: Stream[Number] = Stream.continually(randomInt2)
	    
	    val streamable1: Streamable[Combination] = SingleStream(infiniteStream1, true)
	    val streamable2: Streamable[Number] = SingleStream(infiniteStream2, true)
	    
	    {
		    val bs = BinaryStream(streamable1, streamable2) {
		      case (x: NumberList, y: Number) => NumberList(x.list :+ y)
		      case (x: Number, y: Number) => NumberList(List(x, y))
	      }
		    
		    val leftStream = bs.leftStream
		    val leftStreamString = leftStream.take(10).toList mkString (", ")
		    
		    var currentStream = leftStream
		    val prints = printStream(currentStream)(_)	
    
        compareCallsToGetStream( bs )	    
		    
		    assertTrue(bs.isInfinite)
		    assertEquals(
	        prints(10),
	        Stream.continually(List(randomInt1, randomInt2)).take(10) map
	        	{ x => NumberList(x map { Number(_) }) },
	        currentStream.take(10).toList
	      )
	      currentStream = currentStream.drop(210)
	      assertEquals(
	        prints(10),
	        Stream.continually(List(randomInt1, randomInt2)).take(10) map
	        	{ x => NumberList(x map { Number(_) }) },
	        currentStream.take(10).toList
	      )
    
        compareCallsToGetStream( bs )
	    }
      
	    {
		    val bs = BinaryStream(streamable1, streamable2) {
		      case (x: NumberList, y: Number) => NumberList(x.list :+ y)	      
		      case (x: Number, y: Number) => NumberList(List(x, y))
	      }
		    
		    val rightStream = bs.rightStream
		    val rightStreamString = rightStream.take(10).toList mkString (", ")
		    
		    var currentStream = rightStream
		    val prints = printStream(currentStream)(_)	
    
        compareCallsToGetStream( bs )
		    
		    assertTrue(bs.isInfinite)
		    assertEquals(
	        prints(10),
	        Stream.continually(List(randomInt1, randomInt2)).take(10) map
	        	{ x => NumberList(x map { Number(_) }) },
	        currentStream.take(10).toList
	      )
	      currentStream = currentStream.drop(210)
		    assertEquals(
	        prints(10),
	        Stream.continually(List(randomInt1, randomInt2)).take(10) map
	        	{ x => NumberList(x map { Number(_) }) },
	        currentStream.take(10).toList
	      )
    
        compareCallsToGetStream( bs )
	    }
	    
	    {
		    val bs = BinaryStream(streamable1, streamable2) {
		      case (x: NumberList, y: Number) => NumberList(x.list :+ y)	      
		      case (x: Number, y: Number) => NumberList(List(x, y))
	      }
		    
		    val stream = bs.getStream
		    val streamString = stream.take(10).toList mkString (", ")
		    
		    var currentStream = stream
		    val prints = printStream(currentStream)(_)	
    
        compareCallsToGetStream( bs )
		    
		    assertTrue(bs.isInfinite)
		    assertEquals(
	        prints(10),
	        Stream.continually(List(randomInt1, randomInt2)).take(10) map
	        	{ x => NumberList(x map { Number(_) }) },
	        currentStream.take(10).toList
	      )
	      currentStream = currentStream.drop(210)
		    assertEquals(
	        prints(10),
	        Stream.continually(List(randomInt1, randomInt2)).take(10) map
	        	{ x => NumberList(x map { Number(_) }) },
	        currentStream.take(10).toList
	      )
    
        compareCallsToGetStream( bs )
	    }
	    
    }    
  }
    
  @Test
  def testBinaryStreamOneInfinite { 
    implicit def intToNumber(number: Int) = Number(number)
        
    val rnd = new Random(System.currentTimeMillis())
  	val randomInt1 = rnd.nextInt
    
    {
	    val infiniteStream1: Stream[Combination] = Stream.continually(randomInt1)
	    val finiteStream2 = List[Number](4,5,6).toStream
	    
	    val streamable1: Streamable[Combination] = SingleStream(infiniteStream1, true)
	    val streamable2: Streamable[Number] = SingleStream(finiteStream2, false)
	    
	    {
		    val bs = BinaryStream(streamable1, streamable2) {
		      case (x: NumberList, y: Number) => NumberList(x.list :+ y)
		      case (x: Number, y: Number) => NumberList(List(x, y))
	      }
		    
		    val leftStream = bs.leftStream
		    val leftStreamString = leftStream.take(10).toList mkString (", ")
		    
		    var currentStream = leftStream
		    val prints = printStream(currentStream)(_)		  
		    
		    val fixedList = List(List(randomInt1, 4), List(randomInt1, 4), List(randomInt1, 5), List(randomInt1, 4),
          List(randomInt1, 5), List(randomInt1, 6))
		    
		    assertTrue(bs.isInfinite)
		    assertFalse(currentStream.hasDefiniteSize)
		    assertEquals(
	        prints(10),
	        fixedList map
	        	{ x => NumberList(x map { Number(_) }) },
	        currentStream.take(fixedList.size).toList
	      )
	      currentStream = currentStream.drop(fixedList.size)
	      
	      val fixedListAfter = List(List(randomInt1, 4), List(randomInt1, 5), List(randomInt1, 6))
	      
		    assertTrue(bs.isInfinite)
		    assertFalse(currentStream.hasDefiniteSize)
		    assertEquals(
	        prints(fixedListAfter.size),
	        fixedListAfter map
	        	{ x => NumberList(x map { Number(_) }) },
	        currentStream.take(fixedListAfter.size).toList
	      )
	      
	      currentStream = currentStream.drop(100 * 3)
	      
		    assertEquals(
	        prints(fixedListAfter.size),
	        fixedListAfter map
	        	{ x => NumberList(x map { Number(_) }) },
	        currentStream.take(fixedListAfter.size).toList
	      )
    
        compareCallsToGetStream( bs )
	    }
      
	    {
		    val bs = BinaryStream(streamable1, streamable2) {
		      case (x: NumberList, y: Number) => NumberList(x.list :+ y)	      
		      case (x: Number, y: Number) => NumberList(List(x, y))
	      }
		    
		    val rightStream = bs.rightStream
		    val rightStreamString = rightStream.take(10).toList mkString (", ")
		    		    
		    var currentStream = rightStream
		    val prints = printStream(currentStream)(_)	
		    
		    val fixedList = List(List(randomInt1, 5), List(randomInt1, 6), List(randomInt1, 6))
		    
		    assertTrue(bs.isInfinite)
		    
	      assertEquals(3, rightStream.size)
	      
		    assertEquals(
	        prints(fixedList.size),
	        fixedList map
	        	{ x => NumberList(x map { Number(_) }) },
	        currentStream.take(fixedList.size).toList
	      )
	      currentStream = currentStream.drop(fixedList.size)
	      
	      assertTrue(currentStream.isEmpty)
	      
//	      rightStream.take(200 * fixedList.size)
//		    assertEquals(
//	        rightStreamString,
//	        fixedList map
//	        	{ x => NumberList(x map { Number(_) }) },
//	        rightStream.take(fixedList.size).toList
//	      )
    
        compareCallsToGetStream( bs )
	    }

      {
        val bs = BinaryStream(streamable1, streamable2) {
          case (x: NumberList, y: Number) => NumberList(x.list :+ y)
          case (x: Number, y: Number) => NumberList(List(x, y))
        }

        val stream = bs.getStream
        val streamString = stream.take(10).toList mkString (", ")

        var currentStream = stream
        val prints = printStream(currentStream)(_)

        assertFalse(currentStream.hasDefiniteSize)

        val fixedList = List(
          List(randomInt1, 4), List(randomInt1, 5),
          List(randomInt1, 4), List(randomInt1, 6),
          List(randomInt1, 5), List(randomInt1, 6),
          List(randomInt1, 4), List(randomInt1, 5), List(randomInt1, 6))

        assertTrue(bs.isInfinite)
        assertEquals(
	        prints(fixedList.size),
	        fixedList map
	        	{ x => NumberList(x map { Number(_) }) },
	        currentStream.take(fixedList.size).toList
        )
        
	      currentStream = currentStream.drop(fixedList.size)
	      assertFalse(currentStream.isEmpty)
	      
	      val fixedListAfter = List(List(randomInt1, 4), List(randomInt1, 5), List(randomInt1, 6))
	      
		    assertTrue(bs.isInfinite)
		    assertFalse(currentStream.hasDefiniteSize)
		    assertEquals(
	        prints(fixedListAfter.size),
	        fixedListAfter map
	        	{ x => NumberList(x map { Number(_) }) },
	        currentStream.take(fixedListAfter.size).toList
	      )
	      
	      currentStream = currentStream.drop(100 * fixedListAfter.size)
	      
		    assertEquals(
	        prints(fixedListAfter.size),
	        fixedListAfter map
	        	{ x => NumberList(x map { Number(_) }) },
	        currentStream.take(fixedListAfter.size).toList
	      )
    
        compareCallsToGetStream( bs )
      }
	    
    }    
  }

}