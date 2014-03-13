package insynth
package streams
package benchmarks

import org.scalatest._
import org.scalameter.api._

import dependent._
import streams.{ light => e }

import util._
import util.logging._

class BinarySearchTreeBenchmark extends PerformanceTest.OfflineReport with HasLogger with ProfileLogger {
  import common._
  import Structures._
  import BSTrees._

  performance of "scife enumerators" in {
    measure method "Binary Search Trees" in {
      val sizes = Gen.range("size")(1, 15, 1)
      implicit val memScope = new MemoizationScope
      val enumerator = constructEnumerator

      using(sizes) curve ("Binary Search Trees") warmUp {
        for (size <- 1 to 10) {
          val tdEnumVal = enumerator
          val enum = enumerator.getStream((size, 1 to size))
          for (i <- 0 until enum.size) enum(i)
        }
      } setUp { _ =>
        memScope.clear
      } in { (size: Int) =>
        val tdEnumVal = enumerator
        val enum = enumerator.getStream((size, 1 to size))
        for (i <- 0 until enum.size) enum(i)
      }
    }
  }

  private def constructEnumerator(implicit ms: MemoizationScope) = {
    val rootProducer = Producer[Range, Int](
      (range: Range) => {
        e.WrapperArray(range)
      })

    val sizeProducer = Producer[Int, Int](
      (size: Int) => {
        e.WrapperArray(0 until size)
      })

    Producer.memoized(
      (self: Dependent[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        if (size <= 0) e.Singleton(Leaf)
        else if (size == 1) e.WrapperArray(range map { v => Node(Leaf, v, Leaf) })
        else {
          val roots = rootProducer.getStream(range)
          val leftSizes = sizeProducer.getStream(size)

          val rootLeftSizePairs = e.Binary(leftSizes, roots)

          val leftTrees: Dependent[(Int, Int), Tree] = new InMapper(self, { (par: (Int, Int)) =>
            val (leftSize, median) = par
            (leftSize, range.start to (median - 1))
          })

          val rightTrees: Dependent[(Int, Int), Tree] =
            new InMapper(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (size - leftSize - 1, (median + 1) to range.end)
            })

          val leftRightPairs: Dependent[(Int, Int), (Tree, Tree)] =
            CoupledBinary(leftTrees, rightTrees)

          import BinaryFiniteMemoized._

          val allNodes =
            combine[(Int, Int), (Tree, Tree), Node](rootLeftSizePairs, leftRightPairs,
              (p1: (Int, Int), p2: (Tree, Tree)) => {
                val ((leftSize, currRoot), (leftTree, rightTree)) = (p1, p2)

                Node(leftTree, currRoot, rightTree)
              })

          allNodes
        }
      })
  }

}
