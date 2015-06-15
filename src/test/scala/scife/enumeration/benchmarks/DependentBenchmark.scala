package scife
package enumeration
package benchmarks

import org.scalameter.api._
import org.scalameter.reporting.DsvReporter

import scife.{ enumeration => e }
import dependent._

import scife.util._
import scife.util.logging._

trait DependentMemoizedBenchmark[I, DepEnumType] extends PerformanceTest.OfflineReport
    with java.io.Serializable with HasLogger {

  import structures._
  import memoization.MemoizationScope
  import memoization.scope._

  //  @transient override lazy val reporter = new DsvReporter(',')

  def getScope = new AccumulatingScope

  val defaultContext =
    suite.BenchmarkSuite.contextMinimal
  //Context.empty

  def fixtureRun(
    benchmarkMainName: String,
    name: String,
    maxSize: Int,
    run: String,
    maxSizeWarmup: Option[Int] = None,
    memScope: MemoizationScope = getScope //    ,
    //    constructEnumerator: MemoizationScope => DepEnumType = (ms: MemoizationScope) => this.constructEnumerator(ms),
    //    generator: Int => Gen[I] = this.generator,
    //    warmUp: (DepEnumType, Int) => Any = this.warmUp,
    //    measureCode: DepEnumType => I => Any = this.measureCode,
    //    setUp: (I, DepEnumType, MemoizationScope) => Any = this.setUpFixed
    )(implicit configArguments: org.scalameter.Context = defaultContext) = {
    fixture(benchmarkMainName, name, maxSize, maxSizeWarmup, memScope, Some(run))
  }

  def fixture(
    benchmarkMainName: String,
    name: String,
    maxSize: Int,
    maxSizeWarmup: Option[Int] = None,
    memScope: MemoizationScope = getScope,
    groupName: Option[String] = None //    ,
    //    constructEnumerator: MemoizationScope => DepEnumType = (ms: MemoizationScope) => this.constructEnumerator(ms),
    //    generator: Int => Gen[I] = this.generator,
    //    warmUp: (DepEnumType, Int) => Any = this.warmUp,
    //    measureCode: DepEnumType => I => Any = this.measureCode,
    //    setUp: (I, DepEnumType, MemoizationScope) => Any = this.setUpFixed
    )(
      implicit configArguments: org.scalameter.Context = defaultContext) = {
    require(name != null)
    val warmupSize = maxSizeWarmup.getOrElse(maxSize)
    
    def runWrapper(using: Unit) =
      groupName match {
        case None =>
          using
        case Some(_groupName) =>
          measure method _groupName in using
      }

    performance of benchmarkMainName in {
      runWrapper {
        //        val memScope = new AccumulatingScope
        val enumerator = constructEnumerator(memScope)

        using(generator(maxSize)) config (
          configArguments) curve (name) warmUp {
            System.gc()
            System.gc()
            warmUp(enumerator, warmupSize)
            System.gc()
            System.gc()
          } setUp {
            setUpFixed(_, enumerator, memScope)
          } tearDown {
            tearDownFixed(_, enumerator, memScope)
          } in measureCode(enumerator)
      }
    }
  }

  //  def getUsing(generator: Gen[I], enumerator: DepEnumType, memScope: MemoizationScope): super.Using[I] =
  //    using(generator) config (
  //      exec.jvmcmd -> javaCommand,
  //      exec.jvmflags -> JVMFlags.mkString(" ")
  //    ) curve (name) warmUp {
  //      warmUp(enumerator)
  //    } setUp {
  //      setUpFixed(_, enumerator, memScope)
  //    } tearDown {
  //      tearDownFixed(_, enumerator, memScope)
  //    }

  def measureCode(tdEnum: DepEnumType): I => _

  def generator(maxSize: Int): Gen[I]

  def warmUp(tdEnum: DepEnumType, maxSize: Int): Any

  def setUp(i: I, tdEnum: DepEnumType, memScope: MemoizationScope) {}

  def setUpFixed(i: I, tdEnum: DepEnumType, memScope: MemoizationScope) {
    setUp(i: I, tdEnum: DepEnumType, memScope: MemoizationScope)
    System.gc
    System.gc
    System.gc
    memScope.clear
    System.gc
    System.gc
    System.gc
    info("[DependentBenchmark:] Begin run")
  }

  def tearDown(i: I, tdEnum: DepEnumType, memScope: MemoizationScope): Unit = {}

  final def tearDownFixed(i: I, tdEnum: DepEnumType, memScope: MemoizationScope) {
    tearDown(i, tdEnum, memScope)
    System.gc
    System.gc
    info("[DependentBenchmark:] End run")
  }

  def constructEnumerator(implicit ms: MemoizationScope): DepEnumType

  //  @transient override lazy val reporter = new LoggingReporter

}
