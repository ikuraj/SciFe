package scife.enumeration.member
package benchmarks

import org.scalameter._

import reporting._
import execution._
import Key._

package suite {
  import BenchmarkSuite._
  import scife.enumeration.benchmarks.suite.BenchmarkSuite.{
    allBenchmarks => _,
    _
  }

  class BenchmarkSuiteMinimal extends PerformanceTest.OfflineReport {
    override def persistor = new persistence.SerializationPersistor

    implicit val configArguments = configArgumentsFull

    for (((benchmark, name, _), maxSize) <- allBenchmarks zip minimalSizes)
      benchmark.fixture("Minimal benchmarks", name, maxSize)

  }

  class BenchmarkSuiteFull extends PerformanceTest.OfflineReport {

    override def executor = SeparateJvmsExecutor(
      Executor.Warmer.Default(),
      Aggregator.min,
      new Executor.Measurer.Default)

    implicit val configArguments = configArgumentsFull

    for ((benchmark, name, maxSize) <- allBenchmarks)
      benchmark.fixtureRun(benchmarkMainName, "SciFe", maxSize, name)

  }

}

object BenchmarkSuite {

  val allBenchmarks = List(
    //    (new RedBlackTreeBenchmarkMember, "Red-Black Trees, membership")
    //    ,
    //    (new RedBlackTreeBenchmarkVerify, "Red-Black Trees, invariant execution")
    (new BinarySearchTreeMember, "Binary Search Trees, membership", 15),
    (new BinarySearchTreeVerify, "Binary Search Trees, invariant execution", 15))

}
