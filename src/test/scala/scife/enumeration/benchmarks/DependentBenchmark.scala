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

  import Structures._
  import memoization.MemoizationScope
  import memoization.scope._

//  @transient override lazy val reporter = new DsvReporter(',')

  val defaultContext = Context.empty

  def fixtureRun(
    benchmarkMainName: String,
    name: String,
    maxSize: Int,
    run: String,
    maxSizeWarmup: Option[Int] = None
//    ,
//    constructEnumerator: MemoizationScope => DepEnumType = (ms: MemoizationScope) => this.constructEnumerator(ms),
//    generator: Int => Gen[I] = this.generator,
//    warmUp: (DepEnumType, Int) => Any = this.warmUp,
//    measureCode: DepEnumType => I => Any = this.measureCode,
//    setUp: (I, DepEnumType, MemoizationScope) => Any = this.setUpFixed
    )(
      implicit configArguments: org.scalameter.Context = defaultContext
    ) = {
    require(name != null)
    val warmupSize = maxSizeWarmup.getOrElse(maxSize)

    performance of benchmarkMainName in {
        measure method run in {
          val memScope = new AccumulatingScope
          val enumerator = constructEnumerator(memScope)
  
            using( generator(maxSize) ) config (
              configArguments
            ) curve (name) warmUp {
  System.gc()
  System.gc()
              warmUp(enumerator, warmupSize)
  System.gc()
  System.gc()
            } setUp {
              setUp(_, enumerator, memScope)
            } tearDown {
              tearDownFixed(_, enumerator, memScope)
            } in measureCode( enumerator )
        }
    }
  }

  def fixture(
    benchmarkMainName: String,
    name: String,
    maxSize: Int,
    maxSizeWarmup: Option[Int] = None
//    ,
//    constructEnumerator: MemoizationScope => DepEnumType = (ms: MemoizationScope) => this.constructEnumerator(ms),
//    generator: Int => Gen[I] = this.generator,
//    warmUp: (DepEnumType, Int) => Any = this.warmUp,
//    measureCode: DepEnumType => I => Any = this.measureCode,
//    setUp: (I, DepEnumType, MemoizationScope) => Any = this.setUpFixed
    )(
      implicit configArguments: org.scalameter.Context = defaultContext
    ) = {
    require(name != null)
    val warmupSize = maxSizeWarmup.getOrElse(maxSize)

    performance of benchmarkMainName in {
        val memScope = new AccumulatingScope
        val enumerator = constructEnumerator(memScope)

          using( generator(maxSize) ) config (
            configArguments
          ) curve (name) warmUp {
System.gc()
System.gc()
            warmUp(enumerator, warmupSize)
System.gc()
System.gc()
          } setUp {
            setUp(_, enumerator, memScope)
          } tearDown {
            tearDownFixed(_, enumerator, memScope)
          } in measureCode( enumerator )
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

  final def setUpFixed(i: I, tdEnum: DepEnumType, memScope: MemoizationScope) {
    setUp(i: I, tdEnum: DepEnumType, memScope: MemoizationScope)
//    System.gc
    memScope.clear
//    System.gc
//    System.gc
    info("[DependentBenchmark:] Begin run")
  }
  
  def tearDown(i: I, tdEnum: DepEnumType, memScope: MemoizationScope): Unit = {}
  
  final def tearDownFixed(i: I, tdEnum: DepEnumType, memScope: MemoizationScope) {
    tearDown(i, tdEnum, memScope)
    info("[DependentBenchmark:] End run")
  }

  def constructEnumerator(implicit ms: MemoizationScope): DepEnumType

//  @transient override lazy val reporter = new LoggingReporter

}
