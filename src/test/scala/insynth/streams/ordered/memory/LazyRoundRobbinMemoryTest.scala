package insynth
package streams.ordered
package memory

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import util._

import scala.language.postfixOps

class LazyRoundRobbinMemoryTest extends FunSuite with ShouldMatchers {
  
  import Checks._
  
  def finiteStream = {
    WrapperStream( Seq(new Integer(5), new Integer(5), new Integer(5)) zipWithIndex )
  }
  
  test("Finite streams") {
    var streamable =
      LazyRoundRobbin(
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

  test("One unary stream") {
    val streamable =
      LazyRoundRobbin(
        List(finiteStream) map { e => UnaryStream(e, { (v: Integer) => v + 1 } ) }
      )
    streamable.size should be (3)
    
    assertCollected(streamable)
  }
  
  test("Unary streams") {
    val streamable =
      LazyRoundRobbin(
        List(finiteStream, finiteStream, finiteStream) map { e => UnaryStream(e, { (v: Integer) => v + 1 } ) }
      )
    
    assertCollected(streamable)
  }
  
  test("Unary streams, recursion") {
    val streamable =
      LazyRoundRobbin(
        List(finiteStream, finiteStream, finiteStream) map { e => UnaryStream(e, { (v: Integer) => v + 1 } ) }
      )
      
    streamable addStreamable streamable
    assertCollected(streamable, 100)
  }
  
  ignore("what to do with filters") {
  test("Unary streams, recursion, filter") {
    val streamable =
      LazyRoundRobbin(
        List(finiteStream, finiteStream, finiteStream) map { e => UnaryStream(e, { (v: Integer) => v + 1 } ) }
      )
      
    streamable addStreamable streamable
    assertNotCollected(streamable, 100)
  }
  }

  test("Memoized") {
    val streamable =
      LazyRoundRobbin.memoized(
        List(finiteStream, finiteStream, finiteStream) map { e => UnaryStream(e, { (v: Integer) => v + 1 } ) }
      )

    assertNotCollected(streamable)
  }

}