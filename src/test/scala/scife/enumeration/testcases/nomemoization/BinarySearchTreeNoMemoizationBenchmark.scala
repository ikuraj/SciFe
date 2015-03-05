package scife
package enumeration
package testcases
package nomemoization

import dependent._
import scife.{ enumeration => e }

import scife.enumeration.benchmarks._

import scife.util._
import structures._
import BSTrees._
import logging._

import org.scalatest._
import org.scalameter.api._

import scala.language.existentials

class BinarySearchTreeNoMemoizationBenchmarkTest extends FunSuite with ProfileLogger {

  ignore("Moved to benchmarks - invoke 'bench slow'") {
  for (size <- 1 to 15) {
    test("Enumerating BST without memoization for size " + size) {
      import concurrent.Timeouts._
      import time.SpanSugar._

      val tdEnum =
        (BinarySearchTreeNoMemoizationBenchmark).constructEnumerator(null)

      profile("BST, no memoization, for size " + size) {
        cancelAfter(20 seconds) {
          val enum = tdEnum.getEnum((size, 1 to size))
          for (i <- 0 until enum.size) enum(i)
        }
      }
    }
  }
  }

//  test("No memoization, Binary Search Tree") {
//
//    implicit val configArguments =
//      org.scalameter.Context(
//        exec.maxWarmupRuns -> 2,
//        exec.benchRuns -> 3,
//        exec.independentSamples -> 1
//      )
//
//    (BinarySearchTreeNoMemoizationBenchmark).fixture("NoMemoize", "BinarySearchTree", 8)
//  }

}

object BinarySearchTreeNoMemoizationBenchmark
  extends StructuresBenchmark[Depend[(Int, Range), Tree]] {

  override def executor = SeparateJvmsExecutor(
    Executor.Warmer.Default(),
    Aggregator.average,
    new Executor.Measurer.Default
  )

  type EnumType = Depend[(Int, Range), Tree]

  override def measureCode(tdEnum: EnumType) = {
    { (size: Int) =>
      val enum = tdEnum.getEnum((size, 1 to size))
      for (i <- 0 until enum.size) enum(i)
    }
  }

  override def warmUp(inEnum: EnumType, maxSize: Int) {
    for (size <- 1 to maxSize) {
      val enum = inEnum.getEnum((size, 1 to size))
      for (i <- 0 until enum.size) enum(i)
    }
  }

  override def constructEnumerator(implicit ms: memoization.MemoizationScope) = {
    Depend(
      (self: Depend[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        if (size <= 0) e.Singleton(Leaf): Finite[Tree]
        else if (size == 1)
          e.WrapArray(range map { v => Node(Leaf, v, Leaf) }): Finite[Tree]
        else {
          val roots = e.Enum(range)
          val leftSizes = e.Enum(0 until size)

          val rootLeftSizePairs = e.Product(leftSizes, roots)

          val leftTrees: Depend[(Int, Int), Tree] = InMap(self, { (par: (Int, Int)) =>
            val (leftSize, median) = par
            (leftSize, range.start to (median - 1))
          })

          val rightTrees: Depend[(Int, Int), Tree] =
            InMap(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (size - leftSize - 1, (median + 1) to range.end)
            })

          val leftRightPairs: Depend[(Int, Int), (Tree, Tree)] =
            Product(leftTrees, rightTrees)

          val allNodes =
            Chain[(Int, Int), (Tree, Tree), Node](rootLeftSizePairs, leftRightPairs,
              (p1: (Int, Int), p2: (Tree, Tree)) => {
                val ((leftSize, currRoot), (leftTree, rightTree)) = (p1, p2)

                Node(leftTree, currRoot, rightTree)
              })

          allNodes: Finite[Tree]
        }
      })
  }

}
