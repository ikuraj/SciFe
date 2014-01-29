package insynth
package streams.ordered
package memory

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import util._

import scala.language.postfixOps

class FilterStreamMemoryTest extends FunSuite with ShouldMatchers {
  
  import Checks._
  
  test("FilterStream, one singleton") {
    
    // need to let go of singleton for Int to be GCed
    var streamable =
      FilterStream(
        Singleton(new Integer(5)), { (_: Integer) => true }
      )
    
    var stream = streamable.getValuedStream
    val it = stream.iterator
    
    val testers =
      for (_ <- 1 to 1) yield MemoryLeak(it.next)
    
    stream = null
    streamable = null
    it.hasNext should be (false)
        
    for (tester <- testers)
      tester.assertCollected
  }
  
  test("FilterStream, finite streams") {
    
    var streamable =
      FilterStream(
        WrapperStream( Seq(new Integer(5), new Integer(5), new Integer(5)) zipWithIndex ), { (_: Integer) => true }
      )
    
    var stream = streamable.getValuedStream
    val it = stream.iterator
    
    val testers =
      for (_ <- 1 to 3) yield MemoryLeak(it.next)
    
    stream = null
    streamable = null
    it.hasNext
        
    for (tester <- testers)
      tester.assertCollected
  }

  test("FilterStream, unary streams, all") {
    val streamable =
      FilterStream(
        UnaryStream(WrapperStream( Seq(new Integer(5), new Integer(5), new Integer(5)) zipWithIndex ),
          { (v: Integer) => new Integer(v + 1) } ), { (_: Integer) => true }
      )
    
    assertCollected(streamable, 3)
  }
  
  test("FilterStream, unary streams, some filtered") {
    val finiteStream = WrapperStream( Seq(new Integer(5), new Integer(6), new Integer(7)) zipWithIndex )

    var list = Seq(new Integer(6), new Integer(7), new Integer(8))
    val testers1 =
      for (el <- list) yield MemoryLeak(el, "Seq")
    var itList = list.iterator
    
    val us = UnaryStream( finiteStream, { (v: Integer) => itList.next } )
    val streamable =
      FilterStream(
        us, { (i: Integer) => i % 2 == 0 }
      )
    
    var stream = streamable.getValuedStream
    val it = stream.iterator
    
    val testers2 = { for (_ <- 1 to 2) yield MemoryLeak(it.next, "FS") }
    
    list = null
    stream = null
    it.hasNext should be (false)
    itList.hasNext should be (false)
    itList = null
        
    for (tester <- (testers1 ++ testers2))
      tester.assertCollected
  }
//
//  test("FilterStream behavior when a single inner stream is left") {
//    
//    def stream = Stream.continually({ new Integer(0) })
//    
//    val it = stream.iterator
//    
//    var itStream: Stream[Integer] = null
//    
//    val testers =
//      {
//        MemoryLeak(it.next) :: {
//          itStream = it.toStream
//          Nil
//        } 
//      }
//    
////    it.hasNext
//        
//    for (tester <- testers)
//      tester.assertCollected
//  }
//
//  test("FilterStream behavior when a single inner stream is left, buffered") {
//    
//    def stream = Stream.continually({ new Integer(0) })
//    
//    val it = stream.iterator.buffered
//    
//    var itStream: Stream[Integer] = null
//    
//    val testers =
//      {
//        MemoryLeak(it.next) :: {
//          itStream = it.toStream
//          Nil
//        } 
//      }
//    
////    it.hasNext
//        
//    for (tester <- testers)
//      tester.assertCollected
//  }
//
//  test("FilterStream, unary streams, with call to Checks") {
//    val streamable =
//      FilterStream(
//        List(finiteStream, finiteStream, finiteStream) map { e => UnaryStream(e, { (v: Integer) => v + 1 } ) }
//      )
//    
//    assertCollected(streamable, 3)
//    assertCollected(streamable)
//    assertCollected(streamable)
//      
//  }
//  
//  test("FilterStream, memoized") {
//    val streamable =
//      FilterStream.memoized(
//        List(finiteStream, finiteStream, finiteStream) map { e => UnaryStream(e, { (v: Integer) => v + 1 } ) }
//      )
//
//    assertNotCollected(streamable)
//  }

}