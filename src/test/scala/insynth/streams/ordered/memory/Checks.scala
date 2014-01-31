package insynth
package streams.ordered
package memory

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import util._

import scala.language.postfixOps

object Checks extends ShouldMatchers {
  
  private def traverseAndAssert[T](streamable: IntegerWeightStreamable[T], num: Int,
    flag: Boolean = true,
    iteratorOperation: (Iterator[IntegerWeightPair[T]]) => _ = { (it: Iterator[IntegerWeightPair[T]]) => it.hasNext },
    whetherToTestFun: IntegerWeightPair[T] => Boolean = (_: IntegerWeightPair[T]) => true,
    lastToIgnoreNum: Int = 0
  ) = {
    
    for (it <- List( streamable.getValuedStream.iterator, streamable.getValuedStream.iterator.buffered )) {
    
      val testers = {
        val limit =
          if (num <= 0) {
            streamable.size should be > 0
            streamable.size
          } else num
          
        for (
          index <- 0 until limit; v = it.next;
          if whetherToTestFun(v) && index < limit - lastToIgnoreNum
        ) yield MemoryLeak(v, "Element: " + v.toString)
      }
      
      iteratorOperation(it)
          
      for (tester <- testers)
        if (flag)
          tester.assertCollected
        else
          tester.isCollected should be (false)
    }
  }
  
  private def traverseAndCount[T](streamable: IntegerWeightStreamable[T], num: Int,
    itOp: (Iterator[IntegerWeightPair[T]]) => _, flag: Boolean, testFun: IntegerWeightPair[T] => Boolean) = {
    
    var count = 0
    for (it <- List( streamable.getValuedStream.iterator.buffered )) {
    
      val testers = {
        val limit =
          if (num <= 0) {
            streamable.size should be > 0
            streamable.size
          } else num
          
        for (index <- 0 until limit; v = it.next; if testFun(v)) yield MemoryLeak(v, "Element: " + v.toString)
      }
      
      itOp(it)
          
      for (tester <- testers)
        if (flag == tester.isCollected) count += 1
    }
    
    count
  }
  
  def assertCollected[T](streamable: IntegerWeightStreamable[T], num: Int = -1,
    itOp: (Iterator[IntegerWeightPair[T]]) => _ = { (it: Iterator[IntegerWeightPair[T]]) => it.hasNext } ) = {
    
    traverseAndAssert(streamable, num,
        iteratorOperation = itOp, flag = true)
  }
  
  def assertNotCollected[T](streamable: IntegerWeightStreamable[T], num: Int = -1,
    itOp: (Iterator[IntegerWeightPair[T]]) => _ = { (it: Iterator[IntegerWeightPair[T]]) => it.hasNext } ) = {
    
    traverseAndAssert(streamable, num,
        iteratorOperation = itOp, flag = false)
  }
  
  def assertCollectedWithExceptions[T](streamable: IntegerWeightStreamable[T],
    num: Int = -1,
    itOp: (Iterator[IntegerWeightPair[T]]) => _ = { (it: Iterator[IntegerWeightPair[T]]) => it.hasNext },
    testFun: T => Boolean = (_: T) => true,
    last: Int = 0
    ) = {
    
    traverseAndAssert(streamable, num,
      iteratorOperation = itOp, flag = true,
      whetherToTestFun = { (x: IntegerWeightPair[T]) => testFun(x._1) },
      lastToIgnoreNum = last
    )
  }
  
  def getCollectedCount[T](streamable: IntegerWeightStreamable[T], num: Int = -1, testFun: T => Boolean = (_: T) => true,
    itOp: (Iterator[IntegerWeightPair[T]]) => _ = { (it: Iterator[IntegerWeightPair[T]]) => it.hasNext }
    ) = {
    
    traverseAndCount(streamable, num, itOp, true, { (x: IntegerWeightPair[T]) => testFun(x._1) })
  }

}