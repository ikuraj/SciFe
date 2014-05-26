package insynth.enumeration.benchmarks

import org.scalatest._
import org.scalameter._

import reporting._
import Key._

package suite {
  // if set, does not run full-blown micro-benchmark test suite; it runs
// a quicker benchmark with less reliable results
  
  class BenchmarkSuiteMinimal extends PerformanceTest.OfflineReport {    
    override def persistor = new persistence.SerializationPersistor
    
    import BenchmarkSuite._
  
    val benchmarks = List(
      (new BinarySearchTreeBenchmark, "Binary Search Trees"),
      (new SortedListDependentBenchmark, "Sorted Lists"),
      (new RedBlackTreeDependentBenchmark, "Red-Black Trees"),
      (new HeapArrayBenchmark, "Heap Arrays")
    )
    
    implicit val configArguments = 
      org.scalameter.Context(
        exec.maxWarmupRuns -> 2,
        exec.benchRuns -> 3, 
        exec.independentSamples -> 1
      )
    
    for( ((benchmark, name), maxSize) <- benchmarks zip minimalSizes)
      benchmark.fixture(name, maxSize)

  }

  class BenchmarkSuiteFull extends PerformanceTest.OfflineReport {    
    override def persistor = new persistence.SerializationPersistor
    
    import BenchmarkSuite._
  
    val benchmarks = List(
      (new BinarySearchTreeBenchmark, "Binary Search Trees"),
      (new SortedListDependentBenchmark, "Sorted Lists"),
      (new RedBlackTreeDependentBenchmark, "Red-Black Trees"),
      (new HeapArrayBenchmark, "Heap Arrays")
    )
    
    implicit val configArguments = 
      org.scalameter.Context(
        exec.maxWarmupRuns -> warmUps,
        exec.benchRuns -> numberOfRuns, 
        exec.independentSamples -> JVMs,
        exec.jvmcmd -> javaCommand,
        exec.jvmflags -> JVMFlags.mkString(" ")
      )
    
    for( ((benchmark, name), maxSize) <- benchmarks zip fullBlownSizes)
      benchmark.fixture(name, maxSize)

  }

}

object BenchmarkSuite { 
  
  var maxSize = 15
  
  val minimalSizes = List(3, 3, 3, 3)
  val fullBlownSizes = List(15, 15, 15, 12)
  
  val sizeBinarySearchTree = 3//15//14
  val sizeSortedList = 3//15//16
  val sizeRedBlackTree = 3//15//20
  val sizeHeapArray = 3//12
  val sizeDAGStructure = 3//10//16
  val sizeHeapArray2 = 3//10

  val warmUps = 8
  val numberOfRuns = 3
  val JVMs = 3

  lazy val javaCommand = "java -server"
  lazy val JVMFlags = List(
    // print important outputs
//    "-XX:+PrintCompilation", "-verbose:gc",
    // compilation
//    "-Xbatch", "--XX:CICompilerCount=1",
//    // optimizations
    "-XX:ReservedCodeCacheSize=512M",
    "-XX:CompileThreshold=100", "-XX:+TieredCompilation",
    "-XX:+AggressiveOpts", "-XX:MaxInlineSize=512",
    // memory
    "-Xms28G", "-Xmx28G"
  )
//  println("JVM FLags: " + JVMFlags.mkString(" "))
  
}
