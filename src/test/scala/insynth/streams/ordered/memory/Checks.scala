package insynth
package streams.ordered
package memory

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import util._

import scala.language.postfixOps

object Checks extends ShouldMatchers {
  
  private def traverseAndAssert[T](streamable: IntegerWeightStreamable[T], num: Int,
    itOp: (Iterator[IntegerWeightPair[T]]) => _, flag: Boolean) = {
    
    for (it <- List( streamable.getValuedStream.iterator, streamable.getValuedStream.iterator.buffered )) {
    
      val testers = {
        val limit =
          if (num <= 0) {
            streamable.size should be > 0
            streamable.size
          } else num
          
        for (index <- 0 until limit) yield MemoryLeak(it.next, "Index of: " + index)
      }
      
      itOp(it)
          
      for (tester <- testers)
        if (flag)
          tester.assertCollected
        else
          tester.isCollected should be (false)
    }
  }
  
  def assertCollected[T](streamable: IntegerWeightStreamable[T], num: Int = -1,
    itOp: (Iterator[IntegerWeightPair[T]]) => _ = { (it: Iterator[IntegerWeightPair[T]]) => it.hasNext } ) = {
    
    traverseAndAssert(streamable, num, itOp, true)
  }
  
  def assertNotCollected[T](streamable: IntegerWeightStreamable[T], num: Int = -1,
    itOp: (Iterator[IntegerWeightPair[T]]) => _ = { (it: Iterator[IntegerWeightPair[T]]) => it.hasNext } ) = {
    
    traverseAndAssert(streamable, num, itOp, false)
  }  

}