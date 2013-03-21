package insynth.streams.ordered

import insynth.streams.ordered.{ SingleStream => _ }

import scala.util.Random

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.Test

// enable implicit conversions
import scala.language.implicitConversions

class BinaryStreamTest extends JUnitSuite {    
  
  import Utils._
  
  @Test
  def testBinaryStreamBefore {    
    val stream1 = getSingleStream(1)        
    val stream2 = getSingleStream(2)
    
    assertFalse(stream1.getStream.isEmpty)
    assertFalse(stream1.getValues.isEmpty)
    assertFalse(stream2.getStream.isEmpty)
    assertFalse(stream2.getValues.isEmpty)
        
    val bs = BinaryStream(stream2, stream1) {
      (x, y) => x * y
    }
    
    val stream = bs.getStream
    
    assertFalse(stream.isEmpty)
    assertEquals(2, stream.head)
    assertEquals(3, bs.getValues.head)
  }
  
  @Test
  def testBinaryWithSingletons {    
    val stream1 = Singleton(1)        
    val stream2 = Singleton(2)
        
    val bs = BinaryStream(stream2, stream1) {
      (x, y) => x * y
    }
    
    val stream = bs.getStream
    
    assertEquals(2, stream.head)
    assertEquals(2, bs.getValues.head)
  }
      
  trait Combination
  
  case class Number(number: Int) extends Combination
  
  case class NumberList(list: List[Combination]) extends Combination
  
  @Test
  def testBinaryStreamBothFinite { 
    implicit def intToNumber(number: Int) = Number(number)
    
    {
	    val finiteStream1 = List[Number](1,4,6) zip List(1, 4, 6)
	    val finiteStream2 = List[Number](2,5,6) zip List(2, 5, 6)
	    
	    val streamable1 = getSingleStream(finiteStream1.toStream)
	    val streamable2 = getSingleStream(finiteStream2.toStream)
	    
	    {
		    val bs = BinaryStream(streamable1, streamable2) {
//		      case (x: NumberList, y: NumberList) => NumberList(x.list ++ y.list)
//		      case (x: Number, y: NumberList) => NumberList(x +: y.list)
//		      case (x: NumberList, y: Number) => NumberList(x.list :+ y)	      
		      case (x: Number, y: Number) => NumberList(List(x, y))
	      }
		    
		    val streamString = (bs.getStream zip bs.getValues).toList mkString (", ")
		    
		    val resultList = List(List(1,2), List(1,5), List(4,2), List(1,6), List(6,2), List(4,5), List(4,6), List(6,5), List(6,6))
		    
		    //val resultList = List(List(1,2), List(1,5), List(1,6), List(4,5), List(4,6), List(6,6))
		    
		    val resultValueList = List(3, 6, 6, 7, 8, 9, 10, 11, 12)
		    
		    assertEquals(streamString, 9, bs.getStream.size)	
		    assertEquals(streamString, 9, bs.getValues.size)
		    
        // NOTE this tests fairness also
		    // (..., List(1,5), List(4,2), ...)
		    assertFalse(bs.isInfinite)	    	    
		    assertEquals(
	        streamString,
	        resultList map
	        	{ x => NumberList(x map { Number(_) }) },
	        bs.getStream.toList
	      )		    	      		    
		        	    
		    assertEquals(
	        streamString,
	        resultList map
	        	{ x => NumberList(x map { Number(_) }) } zip resultValueList,
	        bs.getStream zip bs.getValues
	      )
	    }
	    
    }
  }
    
  @Test
  def testBinaryStreamBothInfinite { 
    implicit def intToNumber(number: Int) = Number(number)
        
    val rnd = new Random(System.currentTimeMillis())
  	val randomInt1 = 3
  	val randomInt2 = 2
    
    {
	    val infiniteStream1: Stream[(Combination, Int)] = Stream.continually((randomInt1, randomInt1))
	    val infiniteStream2: Stream[(Number, Int)] = Stream.continually((randomInt2, randomInt2))
	    
	    val streamable1 = new SingleStream(infiniteStream1, true)
	    val streamable2 = new SingleStream(infiniteStream2, true)
	    	    
	    {
		    val bs = BinaryStream(streamable1, streamable2) {
//		      case (x: NumberList, y: NumberList) => NumberList(x.list ++ y.list)
//		      case (x: Number, y: NumberList) => NumberList(x +: y.list)
		      case (x: NumberList, y: Number) => NumberList(x.list :+ y)	      
		      case (x: Number, y: Number) => NumberList(List(x, y))
	      }
		    
		    val stream = bs.getStream
		    val streamString = stream.take(10).toList mkString (", ")
		    
		    var currentStream = stream
		    val prints = streamToString(currentStream)(_)	
		    
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
	        	{ x => NumberList(x map { Number(_) }) } zip Stream.continually(randomInt1 + randomInt2).take(10),
	        currentStream.take(10) zip bs.getValues.take(10)
	      )
	    }
	    
    }    
  }
  
  def checkOrder[T](list: List[(T, Int)]) =
    list == list.sortBy(_._2)
  
  @Test
  def testBinaryStreamBothInfinite2 { 
    implicit def intToNumber(number: Int) = Number(number)
        
    val rnd = new Random(System.currentTimeMillis())
  	val randomInt1 = 1
  	val randomInt2 = 2
    
    {
	    val infiniteStream1: Stream[(Combination, Int)] =
	      Stream.from(randomInt1) map { int => (int: Combination, int) }
	    val infiniteStream2: Stream[(Number, Int)] =
	      Stream.from(randomInt2, 2) map { int => (int: Number, int) }
	    
	    val streamable1 = new SingleStream(infiniteStream1, true)
	    val streamable2 = new SingleStream(infiniteStream2, true)
	    	    
	    {
		    val bs = BinaryStream(streamable1, streamable2) {
//		      case (x: NumberList, y: NumberList) => NumberList(x.list ++ y.list)
//		      case (x: Number, y: NumberList) => NumberList(x +: y.list)
		      case (x: NumberList, y: Number) => NumberList(x.list :+ y)	      
		      case (x: Number, y: Number) => NumberList(List(x, y))
	      }
		    
		    val stream = bs.getStream
		    val streamString = stream.take(10).toList mkString (", ")
		    		    
		    assertTrue(bs.isInfinite)
		    assertEquals(
	        streamToString(stream)(8),
	        List( List(1, 2), List(2,2), List(1,4), List(3,2), List(2,4), List(4,2), List(3, 4), List(1, 6) ) map
	        	{ x => NumberList(x map { Number(_) }) },
	        stream.take(8).toList
	      )
	      
		    var currentStream = stream zip bs.getValues
		    val prints = streamToString(currentStream)(_)	
		    
	      var ind = 0
	      while (ind < 2000) {
		      currentStream = currentStream.drop(147)
			    assertTrue(
		        prints(10),
		        checkOrder(currentStream.take(10).toList)
		      )
		      ind += 147
	      }
	    }
	    
    }    
  }
    
  @Test
  def testBinaryStreamOneInfinite { 
    implicit def intToNumber(number: Int) = Number(number)
        
    val rnd = new Random(System.currentTimeMillis())
  	val randomInt1 = 10
    
    {
	    val infiniteStream1: Stream[(Combination, Int)] =
	      Stream.from(randomInt1) map { int => (int: Combination, int) }
	    val finiteStream2 = List[Number](1,2, 3).toStream zip List(1, 2, 3)
	    
	    val streamable1 = getSingleStream(infiniteStream1, true)
	    val streamable2 = getSingleStream(finiteStream2, false)
	    
      {
        val bs = BinaryStream(streamable1, streamable2) {
//          case (x: NumberList, y: NumberList) => NumberList(x.list ++ y.list)
//          case (x: Number, y: NumberList) => NumberList(x +: y.list)
          case (x: NumberList, y: Number) => NumberList(x.list :+ y)
          case (x: Number, y: Number) => NumberList(List(x, y))
        }

		    val stream = bs.getStream
		    val streamString = stream.take(10).toList mkString (", ")
		    		    
		    assertTrue(bs.isInfinite)
		    assertEquals(
	        streamToString(stream)(9),
	        List( List(10, 1), List(10, 2), List(11, 1), List(10, 3), List(12, 1), List(11,2),
	            List(13,1), List(12, 2), List(11, 3) ) map
	        	{ x => NumberList(x map { Number(_) }) },
	        stream.take(9)
	      )
	      
		    var currentStream = stream zip bs.getValues
		    val prints = streamToString(currentStream)(_)	
		    
	      var ind = 0
	      while (ind < 2000) {
		      currentStream = currentStream.drop(147)
			    assertTrue(
		        prints(10),
		        checkOrder(currentStream.take(10).toList)
		      )
		      ind += 147
	      }
      }
	    
    }    
  }
    
}