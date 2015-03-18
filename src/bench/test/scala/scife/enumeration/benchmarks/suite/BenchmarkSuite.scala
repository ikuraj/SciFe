package scife.enumeration
package benchmarks
package suite

import benchmarks._

import org.scalameter._

import reporting._
import execution._
import Key._

// if set, does not run full-blown micro-benchmark test suite; it runs
// a quicker benchmark with less reliable results

class BenchmarkSuiteMinimal extends PerformanceTest.OfflineReport {
  override def persistor = new persistence.SerializationPersistor

  import BenchmarkSuite._

  val benchmarks = List(
//    (new BinarySearchTreeBenchmark, "Binary Search Trees"),
//    (new SortedListDependentBenchmark, "Sorted Lists"),
//    (new RedBlackTreeDependentBenchmark, "Red-Black Trees"),
//    (new HeapArrayBenchmark, "Heap Arrays"),
    (new scife.enumeration.parallel.BinarySearchTreeBenchmark(Runtime.getRuntime.availableProcessors/2),
      "Binary Search Trees - parallel")
    )

  implicit val configArguments = contextMinimal

  for (((benchmark, name), maxSize) <- benchmarks zip minimalSizes)
    benchmark.fixture("Minimal benchmarks", name, maxSize)

}

class BenchmarkSuiteFull extends PerformanceTest {
  override def persistor = new persistence.SerializationPersistor

  override def reporter: Reporter =
    new SciFeReporter(
      Reporter.Composite(
        new RegressionReporter(
          RegressionReporter.Tester.OverlapIntervals(),
          RegressionReporter.Historian.Complete()),
        // do not embed data into js
        HtmlReporter(false)))

  def executor = SeparateJvmsExecutor(
    Executor.Warmer.Default(),
    Aggregator.min,
    new Executor.Measurer.Default)

  import BenchmarkSuite._

  implicit val configArguments = configArgumentsFull

  for (((benchmark, name), maxSize) <- allBenchmarks zip allBenchmarksNames zip fullBlownSizes)
    benchmark.fixtureRun(benchmarkMainName, "SciFe", maxSize, name)

  val dummyBenchmark = new DummyBenchmark

  for ((name, maxSize) <- allBenchmarksNames zip fullBlownSizes)
    dummyBenchmark.fixtureRun(benchmarkMainName, "Korat", maxSize, name)

  for ((name, maxSize) <- clpBenchmarksNames zip fullBlownSizes)
    dummyBenchmark.fixtureRun(benchmarkMainName, "CLP", maxSize, name)

}

class BenchmarkSuiteParallel extends PerformanceTest {

  override def executor = SeparateJvmsExecutor(
    Executor.Warmer.Default(),
    Aggregator.min,
    new Executor.Measurer.Default)

  import BenchmarkSuite._

  implicit val configArguments = configArgumentsFull +
    (exec.jvmflags -> (JVMFlags ++ heapSize(60)).mkString(" "))
  
  val parallelBenchmarks =
    new scife.enumeration.parallel.BinarySearchTreeBenchmark(Runtime.getRuntime.availableProcessors/2) :: Nil
    
  val benchmarkNames = "Binary Search Trees - parallel" :: Nil

  val benchmarkSizes = 15 :: Nil
    
  for (threads <- 1 to Runtime.getRuntime.availableProcessors/2) {
    for (size <- benchmarkSizes)
      new scife.enumeration.parallel.BinarySearchTreeBenchmark(threads).
        fixtureRun(benchmarkMainName, "SciFe", size, s"Binary Search Trees - parallel/$threads")
  }

//  for (((benchmark, name), maxSize) <- allBenchmarks zip allBenchmarksNames zip fullBlownSizes)
//    benchmark.fixtureRun(benchmarkMainName, "SciFe", maxSize, name)
    
  //override def reporter = new LoggingReporter
  override def reporter =
    Reporter.Composite(
        new RegressionReporter(
          RegressionReporter.Tester.OverlapIntervals(),
          RegressionReporter.Historian.Complete()),
        // do not embed data into js
        HtmlReporter(false))
  
  override def persistor =
    //Persistor.None
    new persistence.SerializationPersistor

}

// benchmarks for which it may take a while to finish (e.g. ones without memoization)
class BenchmarkSuiteSlow extends PerformanceTest {
  override def persistor = api.Persistor.None

  def executor = SeparateJvmsExecutor(
    Executor.Warmer.Default(),
    Aggregator.min,
    new Executor.Measurer.Default)

  def reporter = new LoggingReporter

  import BenchmarkSuite._

  implicit val configArguments =
    org.scalameter.Context(
      exec.maxWarmupRuns -> 1,
      exec.benchRuns -> 3,
      exec.independentSamples -> 1,
      exec.jvmcmd -> javaCommand,
      exec.jvmflags -> JVMFlags.mkString(" "))

  for (
    (benchmark, name, maxSize) <- List(
      (new nomemoization.BinarySearchTreeBenchmark, "Binary Search Tree", 12),
      (new BinarySearchTreeBenchmark, "Binary Search Tree (w/ mem)", 12))
  ) benchmark.fixtureRun(benchmarkMainName, "SciFe (no memoization)", maxSize, name)

}

class DummyBenchmark extends PerformanceTest.OfflineReport {

  def fixtureRun(
    benchmarkMainName: String,
    name: String,
    maxSize: Int,
    run: String)(
      implicit configArguments: org.scalameter.Context) = {
    require(name != null)

    performance of benchmarkMainName in {
      measure method run in {
        using(Gen.range("size")(1, maxSize, 1)) config (
          configArguments) curve (name) warmUp {
          } in { _ => }
      }
    }
  }
}

object BenchmarkSuite {

  val benchmarkMainName = "Benchmarks"

  val allBenchmarks = List(
    new BinarySearchTreeBenchmark,
    new SortedListDependentBenchmark,
    new RedBlackTreeDependentBenchmark,
    new HeapArrayBenchmark,
    new DAGStructureBenchmark
  )

  val allBenchmarksNames = List(
    "Binary Search Tree",
    "Sorted List",
    "Red-Black Tree",
    "Heap Array",
    "Directed Acyclic Graph",
    "Class-Interface DAG"
  )

  val clpBenchmarksNames = List(
    "Binary Search Tree",
    "Sorted List",
    "Red-Black Tree",
    "Heap Array")

  var maxSize = 15

  // max datastructure size
  val minimalSizes = List(3, 3, 3, 3)
  val fullBlownSizes = List(15, 15, 15, 11, 4)
  // normal executor options
  val warmUps = 8
  val numberOfRuns = 3
  val JVMs = 3

  //  val fullBlownSizes = List(3, 3, 3, 3, 3)
  //  val warmUps = 1; val numberOfRuns = 3; val JVMs = 1

  lazy val javaCommand = "java -server"
  lazy val JVMFlags = List(
    // not sure if we should repeat this flag
    "-server",
    // print important outputs
    //    "-XX:+PrintCompilation",
    // verbose GC
    //    "-verbose:gc", "-XX:+PrintGCTimeStamps", "-XX:+PrintGCDetails",
    // compilation
    "-Xbatch",
    // explicit GC calls we need
    "-XX:-DisableExplicitGC",
    //    "--XX:CICompilerCount=1",
    // optimizations
    "-XX:ReservedCodeCacheSize=512M",
    "-XX:CompileThreshold=10", "-XX:+TieredCompilation",
    "-XX:+AggressiveOpts", "-XX:MaxInlineSize=512",
    // disable adaptive policy
    "-XX:-UseAdaptiveSizePolicy",
    "-XX:MinHeapFreeRatio=80",
    "-XX:MaxHeapFreeRatio=100"
  )

  def heapSize(s: Int) = List(
    // new generation size
    s"-XX:NewSize=${s-2}G",
    s"-Xms${s}G", s"-Xmx${s}G"
  )
  //  println("JVM FLags: " + JVMFlags.mkString(" "))

  val configArgumentsFull =
    org.scalameter.Context(
      exec.maxWarmupRuns -> 3,
      exec.benchRuns -> 3,
      exec.independentSamples -> 1,
      exec.jvmcmd -> javaCommand,
      exec.jvmflags -> (JVMFlags ++ heapSize(32)).mkString(" "))
      
  val contextMinimal =
    org.scalameter.Context(
      exec.maxWarmupRuns -> 2,
      exec.benchRuns -> 3,
      exec.independentSamples -> 1)

}
