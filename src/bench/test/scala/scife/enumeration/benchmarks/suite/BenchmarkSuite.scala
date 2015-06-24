package scife.enumeration
package benchmarks
package suite

import benchmarks._

import org.scalameter._

import reporting._
import execution._
import Key._

/* does not run full-blown micro-benchmark test suite; it runs
 a quicker benchmark with less reliable results */
class BenchmarkSuiteMinimal extends PerformanceTest.OfflineReport {
  override def persistor = new persistence.SerializationPersistor

  import BenchmarkSuite._

  implicit val configArguments = contextMinimal
  
  val allBenchmarks = enumerationBenchmarks ++ featuresBenchmarks 

  for (((name, benchmark, _, minimalSize), benchGroup) <- allBenchmarks)
    benchmark.fixture("SciFe, minimal benchmarks", name, minimalSize, groupName = Some(benchGroup))

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

  val allBenchmarks = enumerationBenchmarks ++ featuresBenchmarks 

  for ( ((name, benchmark, maxSize, _), groupName) <- allBenchmarks)
    benchmark.fixture("SciFe benchmarks", name, maxSize, groupName = Some(groupName))

  // needed for integration benchmarks (which run Korat and CLP)
//  val dummyBenchmark = new DummyBenchmark
//
//  for ((name, maxSize) <- allBenchmarksNames zip fullBlownSizes)
//    dummyBenchmark.fixtureRun(benchmarkMainName, "Korat", maxSize, name)
//
//  for ((name, maxSize) <- clpBenchmarksNames zip fullBlownSizes)
//    dummyBenchmark.fixtureRun(benchmarkMainName, "CLP", maxSize, name)

}

class BenchmarkSuiteParallel extends PerformanceTest {

  override def executor = SeparateJvmsExecutor(
    Executor.Warmer.Default(),
    Aggregator.min,
    new Executor.Measurer.Default)

  import BenchmarkSuite._

  implicit val configArguments = configArgumentsFull +
    (exec.jvmflags -> (JVMFlags ++ heapSize(10)).mkString(" "))
    
  import scife.enumeration.parallel._
    
  val benchmarks = List(
    ("Binary Search Trees - parallel", new BinarySearchTreeBenchmark(_: Int), 15),
    ("Riff Image - parallel", new RiffImage(_: Int), 12)
  )
  
  for (threads <- 1 to Runtime.getRuntime.availableProcessors/2) {
//  for (threads <- 5 to 10) {
    for ((name, benchmark, maxSize) <- benchmarks)
      benchmark(threads).fixtureRun(benchmarkMainName, "SciFe", maxSize, s"$name/$threads")
  }

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
    run: String)(implicit configArguments: org.scalameter.Context) = {
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
  
  val enumerationBenchmarks = List(
    ("Binary Search Trees", new BinarySearchTreeBenchmark, 15, 6),
    ("Binary Search Trees, component definition", new BinarySearchTreeComp, 15, 6),
    ("Sorted Lists", new SortedListDependentBenchmark, 15, 6),
    ("Red-Black Trees", new RedBlackTreeDependentBenchmark, 15, 6),
    ("Red-Black Trees, concise definition", new RedBlackTreeConcise, 15, 6),
    ("Heap Arrays", new HeapArrayBenchmark, 11, 6),
    ("Directed Acyclic Graph", new DAGStructureBenchmark, 8, 3),
    ("Class/Interface Hierarchy", new ClassInterfaceDAGBenchmark, 4, 2),
    ("B-tree", new BTreeTest, 15, 6),
    ("RIFF Format", new RiffImage, 6, 3)
  ) zip Stream.continually("Regular enumeration")
  
  val featuresBenchmarks = List[(String, DependentMemoizedBenchmark[_, _], Int, Int)](
    ("Binary Search Trees rnd", new BinarySearchTreeRandom, 15, 6),
    ("Binary Search Trees rnd, noo", new BinarySearchTreeRandomNoOver, 15, 6),
    ("Binary Search Trees no memoization", new nomemoization.BinarySearchTreeBenchmark, 15, 6),
    ("Binary Search Trees, parallel", new scife.enumeration.parallel.BinarySearchTreeBenchmark(Runtime.getRuntime.availableProcessors/2), 15, 6),
    ("Lazy BST", new scife.enumeration.lazytraversal.BinarySearchTree, 15, 6)
 //    ("Lazy BST", (new scife.enumeration.lazytraversal.BinarySearchTree:
//      StructuresBenchmark[scife.enumeration.dependent.Depend[((Int, Range),
//        scife.enumeration.lazytraversal.LazyEnum[scife.util.structures.LazyBSTrees.Tree]),
//        scife.util.structures.LazyBSTrees.Tree]]), 15, 6),
//    ("Normal BST, testing", new scife.enumeration.lazytraversal.BinarySearchTreeNormal, 15)
//    ,
//    ("Normal BST, testing2", new scife.enumeration.lazytraversal.BinarySearchTreeNormal2, 15, 6),
  ) zip Stream.continually("Features benchmark")

  val clpBenchmarksNames = List(
    "Binary Search Tree",
    "Sorted List",
    "Red-Black Tree",
    "Heap Array")

  var maxSize = 15

  // normal executor options
  val warmUps = 8
  val numberOfRuns = 3
  val JVMs = 3

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
    "-XX:-UseAdaptiveSizePolicy"
//    "-XX:MinHeapFreeRatio=80",
//    "-XX:MaxHeapFreeRatio=100"
  )

  def heapSize(s: Int) = List(
    // new generation size
//    s"-XX:NewSize=${s-2}G",
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
