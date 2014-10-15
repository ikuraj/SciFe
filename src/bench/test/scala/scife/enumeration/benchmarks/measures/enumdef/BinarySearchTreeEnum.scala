package scife
package enumeration
package benchmarks
package measures
package enumdef

import scife.{ enumeration => e }
import scife.util._
import scala.language.postfixOps
import logging._
import org.scalameter.api._
import e.util._

object BinarySearchTreeEnum {

  val MinMaxSize = 5

  val minConfigArguments = //BenchmarkSuite.configArgumentsFull
    org.scalameter.Context(
      exec.maxWarmupRuns -> 1,
      exec.benchRuns -> 3,
      exec.independentSamples -> 3)

  new BinarySearchTreeEnum getMeasurements (MinMaxSize, minConfigArguments)

}

class BinarySearchTreeEnum extends Benchmarker with HasLogger {

  import e.memoization.scope.AccumulatingScope

  def getMeasurements(maxSize: Int, configArguments: Context)(implicit enumDefList: List[(e.memoization.MemoizationScope => e.dependent.Depend[(Int, Range), scife.util.structures.BSTrees.Tree], String)] = e.common.enumdef.BinarySearchTreeEnum.enumDefList) = {
    for ((enumDef, name) <- enumDefList) {
      performance of "Binary Search Tree" in {
        performance of "Enumerator definition measurments" in {
          //        measure method name in {
          implicit val memScope = new AccumulatingScope
          val enumerator = enumDef(memScope)

          using(Gen.range("size")(1, maxSize, 1)) config (
            configArguments) curve (name) warmUp {
              info("Warmup started")
              for (size <- 1 to maxSize) {
                info(s"Warming up size ${size}")
                val enum = enumerator.getEnum((size, 1 to size))
                for (i <- 0 until enum.size) enum(i)
              }
              info("Warmup ended")
            } setUp {
              _ =>
                memScope.clear
                info("Calling GC explicitly")
                tryToFreeUpSpace(28)
                info("Explicit GC call done")
                info(s"Measuring ${name} for sizes up to ${maxSize}")
                System.out.flush
            } in {
              (size: Int) =>
                val enum = enumerator.getEnum((size, 1 to size))
                for (i <- 0 until enum.size) enum(i)
            }
          //        }
        }
      }
    }
  }

  private def tryToFreeUpSpace(bigBufferSize: Int) = {
    import java.lang.management._

    val memoryBean = ManagementFactory.getMemoryMXBean();
    System.gc; System.gc
    // try to force GC
    try {
//      @transient
//      var bigBuffer = Array.ofDim[Byte](bigBufferSize * 1024, 1024 * 1024)
//      System.gc; System.gc
//      bigBuffer = null
      System.gc; System.gc
    } catch {
      case e: OutOfMemoryError =>
        val heapUsage = memoryBean.getHeapMemoryUsage();
        val maxMemory = heapUsage.getMax() / (1024 * 1024);
        val usedMemory = heapUsage.getUsed() / (1024 * 1024);
        println(" : Memory Use :" + usedMemory + "M/" + maxMemory + "M");
      case e: Exception =>
        e.printStackTrace();
    }
    //                for (_ <- 1 to 3) {
    Thread.sleep(1000)
    System.gc; System.gc
    //                }
  }

}
