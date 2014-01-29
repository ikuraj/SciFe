package insynth
package streams.ordered
package memory

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import util._

import scala.language.postfixOps

class UnaryStreamMemoryTest extends FunSuite with ShouldMatchers {
  
  def finiteStream = {
    WrapperStream( Seq(new Integer(5), new Integer(5), new Integer(5)) zipWithIndex )
  }
  
  test("UnaryStream") {
    
    val streamable = UnaryStream( finiteStream, { (_: Integer) + 1 } )
    
    var stream = streamable.getValuedStream
    val it = stream.iterator
    
    val testers =
      List( it.next, it.next, it.next ) map {
        MemoryLeak(_)
      }
    
    stream = null
    it.hasNext
        
    for (tester <- testers)
      tester.assertCollected
  }
  
  test("UnaryStream, memoized") {
    
    val streamable = UnaryStream.memoized( finiteStream, { (_: Integer) + 1 } )
    
    var stream = streamable.getValuedStream
    val it = stream.iterator
    
    val testers =
      List( it.next, it.next, it.next ) map {
        MemoryLeak(_)
      }
    
    stream = null
    it.hasNext
        
    for (tester <- testers)
      tester.isCollected should be (false)
  }
  
  test("A reference to inner wrapper stream will prevent those values from being GCed") {
    var finiteStream = WrapperStream( Seq(new Integer(5), new Integer(6), new Integer(7)) zipWithIndex )

    var it = finiteStream.getValuedStream.iterator
    val testers1 =
      for (i <- 0 until 3) yield MemoryLeak(it.next, "FS:"+i)

    val streamable =
      UnaryStream( finiteStream, { (v: Integer) => new Integer(v + 1) } )
    
    var stream = streamable.getValuedStream
    it = stream.iterator
    
    val testers2 =  { for (i <- 0 until 3) yield MemoryLeak(it.next, "US:"+i) }
    stream = null

    finiteStream = null
    it.hasNext should be (false)
        
    for (tester <- testers1)
      tester.isCollected should be (false)
    for (tester <- testers2)
      tester.assertCollected
  }
  
  test("Inner objects produced from a list") {
    val finiteStream = WrapperStream( Seq(new Integer(5), new Integer(6), new Integer(7)) zipWithIndex )

    var list = Seq(new Integer(6), new Integer(7), new Integer(8))
    val testers1 =
      for (el <- list) yield MemoryLeak(el, "Seq")
    var itList = list.iterator
    
    val streamable = UnaryStream( finiteStream, { (v: Integer) => itList.next } )
    
    var stream = streamable.getValuedStream
    val it = stream.iterator
    
    val testers2 = { for (_ <- 1 to 3) yield MemoryLeak(it.next, "US") }
    
    list = null
    stream = null
    it.hasNext should be (false)
    itList.hasNext should be (false)
    itList = null
        
    for (tester <- (testers1 ++ testers2))
      tester.assertCollected
  }

}