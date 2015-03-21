package scife.enumeration
package benchmarks
package suite

import benchmarks._

import org.scalameter._

import reporting._
import execution._
import Key._

class BenchmarkSuiteMeasure extends scife.enumeration.util.Benchmarker {

  import benchmarks.measures.enumdef._
  
//  val numberOfJVMs = System.getenv("NumOfJVMs")
//  
//  println(System.getenv("NumOfJVMs"))
//  println(System.getProperty("NumOfJVMs"))
  
  import common.enumdef.BinarySearchTreeEnum._
  import BenchmarkSuite._
  
  implicit val listToTest = { 
    val enumMap = (enumDefList.map(_._2) zip enumDefList).toMap
    List(
      "constructEnumeratorBenchmarkNoTuplesWhenConstructingTree",
      "constructEnumeratorBenchmark"
        , "constructEnumeratorBenchmark_DynamicMemoized"
    ).map(enumMap(_))
  }
  
  val configArguments = configArgumentsFull +
      (exec.jvmflags -> (JVMFlags ++ heapSize(30)).mkString(" ")) +
      (exec.maxWarmupRuns -> 1) + (exec.benchRuns -> 2) +
      (exec.independentSamples -> 2)
  
  new BinarySearchTreeEnum().getMeasurements(15, configArguments, 8)(listToTest)
 
  val NumberOfProcessors = Runtime.getRuntime.availableProcessors 
  for (numOfThreads <- 2 to NumberOfProcessors) {
    val benchmark = new measures.BinarySearchTreeParallelBenchmark(numOfThreads)
    
    val size = 15

//    benchmark.fixtureRun("BinarySearchTree", "SciFe - parallel", size, s"(#treads=$numOfThreads)",
//      Some(size), memScope = new parallel.memoization.scope.AccumulatingConcurrentScope)(
//      configArguments)
  }
    
  override val executor = SeparateJvmsExecutor(
    Executor.Warmer.Default(),
    Aggregator.min,
    new Executor.Measurer.Default)
  
}
