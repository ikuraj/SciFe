package scife
package enumeration
package benchmarks
package test

import dependent._
import memoization._
import scife.{ enumeration => e }
import scife.util._

import scife.util.logging._

import structures._
import BSTrees._

import org.scalatest._
import org.scalameter.api._

import scala.language.existentials

object BinarySearchTreeBenchmarkOld
  extends PerformanceTest.Quickbenchmark
  with Matchers
  with HasLogger with ProfileLogger {

  import structures._
  import BSTrees._

  performance of "scife enumerators" in {
    measure method "Binary Search Trees" in {
      val sizes = Gen.range("size")(1, 5, 1)
      implicit val memScope = new scope.AccumulatingScope
      
      import e.common.enumdef.BinarySearchTreeEnum._
      val enumerator = constructBenchmarkOld

      enumerator shouldBe a [memoization.dependent.Memoized[_, _]]

      using(sizes) curve ("Binary Search Trees") warmUp {
        for (size <- 1 to 6) {
          val tdEnumVal = enumerator
          val enum = enumerator.getEnum((size, 1 to size))
          for (i <- 0 until enum.size) enum(i)
        }
      } setUp { _ =>
        memScope.clear
      } in { (size: Int) =>
        val tdEnumVal = enumerator
        val enum = enumerator.getEnum((size, 1 to size))
        for (i <- 0 until enum.size) enum(i)
      }
    }
  }

}
