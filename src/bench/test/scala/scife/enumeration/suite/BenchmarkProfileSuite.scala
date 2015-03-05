package scife.enumeration
package suite

import scife.{ enumeration => e }
import scife.util._

import scala.language.postfixOps

import logging._

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._
import org.scalacheck.Gen

class BenchmarkProfileSuite extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {

  import e.common.enumdef.BinarySearchTreeEnum._
  import e.memoization.scope.AccumulatingScope

  test("Profile", scife.util.tags.SlowTest) {
    
    for (i <- 1 to 3) {
      println(s"Test code starting in ${4 - i}")
      Thread.sleep(1000)
    }
    println("Test code started")

    implicit val memScope = new AccumulatingScope
    val enumerator = constructEnumeratorBenchmarkNoTuplesWhenConstructingTree(memScope)

    for (size <- 10 to 16) {
      info(s"Warming up size ${size}")
      val enum = enumerator.getEnum((size, 1 to size))
      for (i <- 0 until enum.size) enum(i)
    }
    memScope.clear
    
  }

}
