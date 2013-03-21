package insynth.streams.unordered

import scala.util.Random

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.Test

class MemoizationTest extends JUnitSuite {    
  
  val random = new Random(System.currentTimeMillis())
  
  val threeElements = List.fill(3)(random.nextInt(100))
  
  val threeElementStreamable = SingleStream(threeElements.toStream, false) 
  
  def makeAndAddElement(element: Int)(implicit accFun:(Int) => Unit): Int = {
    accFun(element)    
    element
  }
  
  @Test
  def testUnaryStream {   
    
    var myCount = 0
    
    implicit val accFun: Int => Unit = (el: Int) => myCount += el
        
    val us = UnaryStream[Int, Int](threeElementStreamable, makeAndAddElement(_))
    
    for ((streamEl, expEl) <- us.getStream zip threeElements) {
      assertEquals(expEl, streamEl)
    }
    
    for ((streamEl, expEl) <- us.getStream zip threeElements) {
      assertEquals(expEl, streamEl)
    }
    
    us.getStream(0)
    us.getStream(2)
    
    assertEquals(threeElements.sum, myCount)
  }
}