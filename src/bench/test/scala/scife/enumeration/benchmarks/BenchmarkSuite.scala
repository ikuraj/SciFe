package scife.enumeration.benchmarks

import org.scalameter._

import reporting._
import execution._
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
      benchmark.fixture("Minimal benchmarks", name, maxSize)

  }

  class BenchmarkSuiteFull extends PerformanceTest {
    override def persistor = new persistence.SerializationPersistor

    override def reporter: Reporter =
      new SciFeReporter(
        Reporter.Composite(
        new RegressionReporter(
          RegressionReporter.Tester.OverlapIntervals(),
          RegressionReporter.Historian.Complete() ),
        // do not embed data into js
        HtmlReporter(false)
        )
      )

    def executor = SeparateJvmsExecutor(
      Executor.Warmer.Default(),
      Aggregator.average,
      new Executor.Measurer.Default
    )

    import BenchmarkSuite._

    implicit val configArguments =
      org.scalameter.Context(
        exec.maxWarmupRuns -> warmUps,
        exec.benchRuns -> numberOfRuns,
        exec.independentSamples -> JVMs,
        exec.jvmcmd -> javaCommand,
        exec.jvmflags -> JVMFlags.mkString(" ")
      )

    for( ((benchmark, name), maxSize) <- allBenchmarks zip allBenchmarksNames zip fullBlownSizes)
      benchmark.fixtureRun(benchmarkMainName, "SciFe", maxSize, name)

    val dummyBenchmark = new DummyBenchmark

    for( (name, maxSize) <- allBenchmarksNames zip fullBlownSizes)
      dummyBenchmark.fixtureRun(benchmarkMainName, "Korat", maxSize, name)

    for( (name, maxSize) <- clpBenchmarksNames zip fullBlownSizes)
      dummyBenchmark.fixtureRun(benchmarkMainName, "CLP", maxSize, name)

  }

  class DummyBenchmark extends PerformanceTest.OfflineReport {

    def fixtureRun(
      benchmarkMainName: String,
      name: String,
      maxSize: Int,
      run: String
      )(
        implicit configArguments: org.scalameter.Context
      ) = {
      require(name != null)

      performance of benchmarkMainName in {
        measure method run in {
          using( Gen.range("size")(1, maxSize, 1) ) config (
            configArguments
          ) curve (name) warmUp {
          } in { _ => }
        }
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
    "Heap Array"
  )

  var maxSize = 15

  val minimalSizes = List(3, 3, 3, 3)

  val fullBlownSizes = List(15, 15, 15, 11, 4)
  val warmUps = 8
  val numberOfRuns = 3
  val JVMs = 3

//  val fullBlownSizes = List(3, 3, 3, 3, 3)
//  val warmUps = 1; val numberOfRuns = 3; val JVMs = 1

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
    "-Xms32G", "-Xmx32G"
  )
//  println("JVM FLags: " + JVMFlags.mkString(" "))

}
