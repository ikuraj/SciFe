package insynth
package streams.ordered
package memory

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import util._

class SingletonMemoryTest extends FunSuite with ShouldMatchers {

  test("Singleton vars") {
    
    def singleton = Singleton(1)
    
    singleton.getValuedStream.head should be ( (1, 1) )
    
    val testers =
      List( singleton, singleton.getValuedStream.head ) map {
        MemoryLeak(_)
      }
        
    for (tester <- testers)
      tester.assertCollected
  }

  test("Singleton iterator, null") {
    
    def singleton = Singleton(1)
    
    var iterator = singleton.getValuedStream.iterator
    
    val testers =
      List( iterator.next ) map {
        MemoryLeak(_)
      }
    
    iterator = null
        
    for (tester <- testers)
      tester.assertCollected
  }
  
  test("Singleton iterator, hasNext") {
    
    def singleton = Singleton(1)
    
    var iterator = singleton.getValuedStream.iterator
    
    val testers =
      List( iterator.next ) map {
        MemoryLeak(_)
      }
    
    iterator.hasNext should be (false)
    
    for (tester <- testers)
      tester.assertCollected
  }
  
  test("Singleton iterator, not collected") {
    
    def singleton = Singleton(1)
    
    var iterator = singleton.getValuedStream.iterator
    
    val testers =
      List( iterator.next ) map {
        MemoryLeak(_)
      }
    
    for (tester <- testers)
      tester.isCollected should be (false)
  }
  
  test("Singleton stream") {
    
    def singleton = Singleton(1).getValuedStream
    
    var str = singleton
    
    val testers =
      List( str.head ) map {
        MemoryLeak(_)
      }
    
    str = str.tail
    
    for (tester <- testers)
      tester.assertCollected
  }

}