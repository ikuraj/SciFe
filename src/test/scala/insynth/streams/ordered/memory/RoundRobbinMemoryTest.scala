package insynth
package streams.ordered
package memory

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import util._

import scala.language.postfixOps

class RoundRobbinMemoryTest extends FunSuite with ShouldMatchers {
  
  import Checks._
  
  def finiteStream = {
    WrapperStream( Seq(new Integer(5), new Integer(5), new Integer(5)) zipWithIndex )
  }
  
  test("RoundRobbin, one singleton") {
    
    // need to let go of singleton for Int to be GCed
    var streamable =
      RoundRobbin(
        List(Singleton(new Integer(5)))
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
  
  test("RoundRobbin, finite streams") {
    
    var streamable =
      RoundRobbin(
        List(finiteStream, finiteStream, finiteStream)
      )
    
    var stream = streamable.getValuedStream
    val it = stream.iterator
    
    val testers =
      for (_ <- 1 to 9) yield MemoryLeak(it.next)
    
    stream = null
    streamable = null
    it.hasNext
        
    testers.head.assertCollected
    for (tester <- testers.take(1))
      tester.assertCollected
  }
  
  test("RoundRobbin, unary streams, one value") {
    
    val streamable =
      RoundRobbin(
        List(finiteStream, finiteStream, finiteStream) map { e => UnaryStream(e, { (v: Integer) => v + 1 } ) }
      )
    
    var stream = streamable.getValuedStream
    val it = stream.iterator
    
    val testers =
      for (_ <- 1 to 9) yield MemoryLeak(it.next)
    
    stream = null
    it.hasNext
        
    testers.head.assertCollected
    for (tester <- testers.take(1))
      tester.assertCollected
  }

  test("RoundRobbin, unary streams, all") {
    val streamable =
      RoundRobbin(
        List(finiteStream, finiteStream, finiteStream) map { e => UnaryStream(e, { (v: Integer) => v + 1 } ) }
      )
    
    var stream = streamable.getValuedStream
    val it = stream.iterator
    
    val testers =
      for (_ <- 1 to 9) yield MemoryLeak(it.next)
    
    stream = null
    it.hasNext
        
    testers.head.assertCollected
    for (tester <- testers)
      tester.assertCollected
      
  }

  test("RoundRobbin behavior when a single inner stream is left") {
    
    def stream = Stream.continually({ new Integer(0) })
    
    val it = stream.iterator
    
    var itStream: Stream[Integer] = null
    
    val testers =
      {
        MemoryLeak(it.next) :: {
          itStream = it.toStream
          Nil
        } 
      }
    
//    it.hasNext
        
    for (tester <- testers)
      tester.assertCollected
  }

  test("RoundRobbin behavior when a single inner stream is left, buffered") {
    
    def stream = Stream.continually({ new Integer(0) })
    
    val it = stream.iterator.buffered
    
    var itStream: Stream[Integer] = null
    
    val testers =
      {
        MemoryLeak(it.next) :: {
          itStream = it.toStream
          Nil
        } 
      }
    
//    it.hasNext
        
    for (tester <- testers)
      tester.assertCollected
  }

  test("RoundRobbin, unary streams, with call to Checks") {
    val streamable =
      RoundRobbin(
        List(finiteStream, finiteStream, finiteStream) map { e => UnaryStream(e, { (v: Integer) => v + 1 } ) }
      )
    
    assertCollected(streamable, 3)
    assertCollected(streamable)
    assertCollected(streamable)
      
  }
  
  test("RoundRobbin, memoized") {
    val streamable =
      RoundRobbin.memoized(
        List(finiteStream, finiteStream, finiteStream) map { e => UnaryStream(e, { (v: Integer) => v + 1 } ) }
      )

    assertNotCollected(streamable)
  }

}