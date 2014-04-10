package insynth
package enumeration
package benchmarks

import org.scalatest._
import org.scalameter.api._

import dependent._
import memoization._
import insynth.{ enumeration => e }
import insynth.util._

import insynth.util.logging._

trait DependentMemoizedBenchmark[I, DepEnumType] extends PerformanceTest.OfflineReport
  with java.io.Serializable with HasLogger {
  import Structures._

  val benchmarkMainName = "SciFe_Dependent_Enumerators"

  lazy val javaCommand = "java -server"
  lazy val JVMFlags = List(
    // print important outputs
    "-XX:+PrintCompilation", "-verbose:gc",
    // compilation
//    "-Xbatch", "--XX:CICompilerCount=1",
//    // optimizations
    "-XX:ReservedCodeCacheSize=512M",
    "-XX:CompileThreshold=100", "-XX:+TieredCompilation",
    "-XX:+AggressiveOpts", "-XX:MaxInlineSize=512",
    // memory
    "-Xms24G", "-Xmx24G"
  )
//  println("JVM FLags: " + JVMFlags.mkString(" "))

  def fixture: Unit = fixture()

  def fixtureRun(
    run: String,
    constructEnumerator: MemoizationScope => DepEnumType = this.constructEnumerator,
    generator: Gen[I] = this.generator,
    warmUp: DepEnumType => Any = this.warmUp,
    measureCode: (super.Using[I], DepEnumType) => Any = this.measureCode,
    setUp: (I, DepEnumType, MemoizationScope) => Any = this.setUp) =
    performance of benchmarkMainName in {
      performance of name in {
        measure method run in {
          implicit val memScope = new MemoizationScope
          val enumerator = constructEnumerator(memScope)

          measureCode(
            using(generator) config (
              exec.jvmcmd -> javaCommand,
              exec.jvmflags -> JVMFlags.mkString(" ")
            ) curve (name) warmUp {
              warmUp(enumerator)
            } setUp {
              setUpFixed(_, enumerator, memScope)
            } tearDown {
              tearDownFixed(_, enumerator, memScope)
            }, enumerator)
        }
      }
    }

  def fixture(
    constructEnumerator: MemoizationScope => DepEnumType = this.constructEnumerator,
    generator: Gen[I] = this.generator,
    warmUp: DepEnumType => Any = this.warmUp,
    measureCode: (super.Using[I], DepEnumType) => Any = this.measureCode,
    setUp: (I, DepEnumType, MemoizationScope) => Any = this.setUp) =
    performance of benchmarkMainName in {
      performance of name in {
        implicit val memScope = new MemoizationScope
        val enumerator = constructEnumerator(memScope)

        measureCode(
          using(generator) config (
            exec.jvmcmd -> javaCommand,
            exec.jvmflags -> JVMFlags.mkString(" ")
          ) curve (name) warmUp {
            warmUp(enumerator)
          } setUp {
            setUpFixed(_, enumerator, memScope)
          } tearDown {
            tearDownFixed(_, enumerator, memScope)
          }, enumerator)
        //        measureCode(getUsing(generator, enumerator, memScope), enumerator)
      }
    }

  def getUsing(generator: Gen[I], enumerator: DepEnumType, memScope: MemoizationScope): super.Using[I] =
    using(generator) config (
      exec.jvmcmd -> javaCommand,
      exec.jvmflags -> JVMFlags.mkString(" ")
    ) curve (name) warmUp {
      warmUp(enumerator)
    } setUp {
      setUpFixed(_, enumerator, memScope)
    } tearDown {
      tearDownFixed(_, enumerator, memScope)
    }

  def measureCode(using: super.Using[I], tdEnum: DepEnumType): Any

  def name: String

  def generator: Gen[I]

  def warmUp(tdEnum: DepEnumType): Any

  def setUp(i: I, tdEnum: DepEnumType, memScope: MemoizationScope) {}

  final def setUpFixed(i: I, tdEnum: DepEnumType, memScope: MemoizationScope) {
    setUp(i: I, tdEnum: DepEnumType, memScope: MemoizationScope)
    System.gc
    memScope.clear
    info("[DependentBenchmark:] Begin run")
  }

  final def tearDownFixed(i: I, tdEnum: DepEnumType, memScope: MemoizationScope) {
    info("[DependentBenchmark:] End run")
  }

  def constructEnumerator(ms: MemoizationScope): DepEnumType

}