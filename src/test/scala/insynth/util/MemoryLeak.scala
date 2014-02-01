package insynth.util

import java.lang.ref._

/**
 * A simple utility class that can verify that an object has been successfully garbage collected.
 * NOTE: based on http://stackoverflow.com/questions/6749948/automated-memory-leak-detection-in-java
 */
class MemoryLeak[T](values: Traversable[T], val name: String = "variable") {
  
  val tester = new MemoryLeakTester[T]()
  for (v <- values) tester.add(v)
  
  def assertCollected =
    assert(tester.isCollected,
      "Object %s was not collected after %d collection iterations.".format(name, MemoryLeakTester.MAX_GC_ITERATIONS))
      
  def isCollected = tester.isCollected
  
  def countCollected = tester.countCollected
  
}

object MemoryLeak {
    
  def apply[T](value: T) = {
    new MemoryLeak(Seq(value))
  }
    
  def apply[T](value: T, name: String) = {
    new MemoryLeak(Seq(value), name)
  }
    
  def apply[T](values: Traversable[T]) = {
    new MemoryLeak(values)
  }
    
  def apply[T](values: Traversable[T], name: String) = {
    new MemoryLeak(values, name)
  }
  
}