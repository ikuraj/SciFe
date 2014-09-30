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

class BinarySearchTreeEnum extends Benchmarker with HasLogger {

  import e.common.enumdef.BinarySearchTreeEnum._
  import e.memoization.scope.AccumulatingScope

  val MinMaxSize = 5

  val minConfigArguments = //BenchmarkSuite.configArgumentsFull
    org.scalameter.Context(
      exec.maxWarmupRuns -> 1,
      exec.benchRuns -> 3,
      exec.independentSamples -> 1)
      
  getMeasurements(MinMaxSize, minConfigArguments)
      
  def getMeasurements(maxSize: Int, configArguments: Context) = {
    for ((enumDef, name) <- enumDefList) {
      performance of "Binary Search Tree" in {
        performance of "Enumerator definition measurments" in {
  //        measure method name in {
            implicit val memScope = new AccumulatingScope
            val enumerator = enumDef(memScope)
  
            using (Gen.range("size")(1, maxSize, 1)) config (
              configArguments
            ) curve (name) warmUp {
              for (size <- 1 to maxSize) {
                val enum = enumerator.getEnum((size, 1 to size))
                for (i <- 0 until enum.size) enum(i)
              }
            } setUp {
              _ =>
                memScope.clear
                System.gc
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
  
}
