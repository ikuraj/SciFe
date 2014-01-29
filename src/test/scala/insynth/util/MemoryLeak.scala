package insynth.util

import java.lang.ref._

/**
 * A simple utility class that can verify that an object has been successfully garbage collected.
 * NOTE: based on http://stackoverflow.com/questions/6749948/automated-memory-leak-detection-in-java
 */
class MemoryLeak[T](value: T, val name: String = "variable") {
  
  val tester = new MemoryLeakTester(value)
  
  def assertCollected =
    assert(tester.isCollected,
      "Object %s was not collected after %d collection iterations.".format(name, MemoryLeakTester.MAX_GC_ITERATIONS))
      
  def isCollected = tester.isCollected
  
}

object MemoryLeak {
    
  def apply[T](value: T) = {
    new MemoryLeak(value)
  }
    
  def apply[T](value: T, name: String) = {
    new MemoryLeak(value, name)
  }
  
}