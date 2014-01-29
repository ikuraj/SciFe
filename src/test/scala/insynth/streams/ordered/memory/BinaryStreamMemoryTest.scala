package insynth
package streams.ordered
package memory

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import util._

import scala.language.postfixOps

class BinaryStreamMemoryTest extends FunSuite with ShouldMatchers {
  
  import Checks._
  
  def finiteStream = {
    WrapperStream( Seq(new Integer(5), new Integer(5), new Integer(5)) zipWithIndex )
  }
  
  test("Finite streams") {
    var streamable =
      BinaryStream(finiteStream, finiteStream)( { _ + _ } )
    
    var stream = streamable.getValuedStream
    val it = stream.iterator
    
    val testers =
      for (_ <- 1 to 9) yield MemoryLeak(it.next)
    
    stream = null
    streamable = null
    it.hasNext
        
    for (tester <- testers)
      tester.assertCollected
  }

  test("Finite streams, 2nd") {
    val streamable =
      BinaryStream(finiteStream, finiteStream)( { _ + _ } )
    streamable.size should be (9)
    
    assertCollected(streamable)
  }
  
  test("Unary streams") {
    def unaryStream = UnaryStream(finiteStream, { (v: Integer) => v + 1 } )
    val streamable =
      BinaryStream(unaryStream, unaryStream)( { _ + _ } )
    
    assertCollected(streamable)
  }
  
  test("Unary streams, recursion") {
    val streamable =
      LazyRoundRobbin(
        List(finiteStream, finiteStream, finiteStream) map { e => UnaryStream(e, { (v: Integer) => v + 1 } ) }
      )
    val binaryStreamable =
      BinaryStream(finiteStream, streamable)( { _ + _ } )
    streamable addStreamable binaryStreamable
      
    assertCollected(streamable, 100)
  }

  test("Memoized") {
    val streamable =
      BinaryStream.memoized(finiteStream, finiteStream)( { _ + _ } )
    streamable.size should be (9)

    assertNotCollected(streamable)
  }

}