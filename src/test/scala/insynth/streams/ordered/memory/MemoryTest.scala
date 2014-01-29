package insynth
package streams.ordered.memory

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import util._

import scala.language.postfixOps

@RunWith(classOf[JUnitRunner])
class MemoryTest extends FunSuite with ShouldMatchers {

  test("Dummy vars") {
    
    def list = List(1, List(1), 3)
    
    val testers =
      List( list, list(1) ) map {
        MemoryLeak(_)
      }
        
    for (tester <- testers)
      tester.assertCollected
  }

  test("Dummy init block") {
    
    def list = List(1, List(1), 3)
    
    val testers =    
      {
        List( list, list(1) ) map {
          MemoryLeak(_)
        }
      }
        
    for (tester <- testers)
      tester.assertCollected
  }

  test("Dummy init block with val (this does not get collected)") {
    
    def list = List(1, List(1), 3)
    
    val testers =    
      {
        val listVal = list
        
        List( listVal, listVal(1) ) map {
          MemoryLeak(_)
        }
      }
        
    for (tester <- testers)
      tester.isCollected should be (false)
  }

  test("Dummy init block with var") {
    
    def list = List(1, List(1), 3)
    
    val testers =    
      {
        var listVal = list
        
        val res = List( listVal, listVal(1) ) map {
          MemoryLeak(_)
        }
        
        listVal = null
        
        res
      }
        
    for (tester <- testers)
      tester.assertCollected
  }

  test("A value past stream iterator") {
      
    class A
    
    for ( 
      produceIt <- List ( (s: Stream[A]) => s.iterator , (s: Stream[A]) => s.iterator.buffered );
      f <- List ( { (i: Iterator[A]) => i.next }, { (i: Iterator[A]) => i.hasNext }
    )) {
    
	    def stream = Stream.continually({ new A })
	    
	    val it = produceIt(stream)
	    
	    val testers =    
	      {
	        List( it.next ) map {
	          MemoryLeak(_)
	        }
	      }
	    
	    f(it)
	        
	    for (tester <- testers)
	      tester.assertCollected
    }
  }

  test("A value past stream iterator, no next") {
        
    class A
    
    def stream = Stream.continually({ new A })
    
    val it = stream.iterator
    
    val testers =    
      {
        List( it.next ) map {
          MemoryLeak(_)
        }
      }
    
    for (tester <- testers)
      tester.isCollected should be (false)
  }
  
  test("Unary stream behavior - inner objects produced from a list") {
    class ImFabulous

    val finiteStream = Stream(new Integer(5), new Integer(6), new Integer(7)) zipWithIndex

    var list = Stream(new ImFabulous, new ImFabulous, new ImFabulous)
    val testers1 =
      for (el <- list.toList) yield MemoryLeak(el, "Seq")
    val itList = list.iterator
    
    var stream = finiteStream map ( v => (itList.next, v._2) )
    
    val it = stream.iterator
    
    val testers2 = { for (_ <- 1 to 3) yield MemoryLeak(it.next, "US") }
    
    list = null
    stream = null
    it.hasNext should be (false)
    itList.hasNext should be (false)
        
//    Console.readLine()
    
    for (tester <- (testers1 ++ testers2))
      tester.assertCollected
  }
  
  test("Unary stream behavior - inner objects produced from a list, seq to stream") {
    class ImFabulous

    val finiteStream = Seq(new Integer(5), new Integer(6), new Integer(7)) zipWithIndex

    var list = Stream(new ImFabulous, new ImFabulous, new ImFabulous)
    val testers1 =
      for (el <- list.toList) yield MemoryLeak(el, "Seq")
    val itList = list.iterator
    
    var stream = finiteStream map ( v => (itList.next, v._2) ) toStream
    
    val it = stream.iterator
    
    val testers2 = { for (_ <- 1 to 3) yield MemoryLeak(it.next, "US") }
    
    list = null
    stream = null
    it.hasNext should be (false)
    itList.hasNext should be (false)
        
//    Console.readLine()
    
    for (tester <- (testers1 ++ testers2))
      tester.assertCollected
  }
  
  test("Unary stream behavior - inner objects produced from a list, testers seq") {
    class ImFabulous
    
    val finiteStream = Seq((new Integer(4), 0))

    var list = Stream(new ImFabulous)
    val testers =
//      List ( MemoryLeak(list, "Seq") )
      for (el <- list.toList) yield MemoryLeak(el, "Seq")
    var itList = list.iterator
    
    var stream = finiteStream map ( v => (itList.next, v._2) )
    // enumerate until end
    stream.size should be (1)
    
    list = null
    stream = null
    itList.hasNext should be (false)
    // okay, iterators to finite sequences have reference to their collections
//    itList = null
    
        
    for (tester <- testers)
      tester.assertCollected
  }

}