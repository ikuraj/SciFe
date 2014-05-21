package insynth
package enumeration
package benchmarks

import dependent._
import memoization._
import insynth.{ enumeration => e }
import insynth.util._

import insynth.util.logging._

import Structures._
import BSTrees._

import org.scalatest._
import org.scalameter.api._

import scala.language.existentials

class BinarySearchTreeBenchmark
  extends StructuresBenchmark[Depend[(Int, Range), Tree]]
//  extends PerformanceTest.OfflineReport with HasLogger with ProfileLogger
  {
  import common._
  
//  def generator: Gen[Int] =
//    Gen.range("size")(1, this.maxSize, 1)
    
  // NOTE: declare name first - otherwise fixture will use uninitialized field
  override val name = "Binary Search Trees"

  fixture

  type EnumType = Depend[(Int, Range), Tree]

  override def maxSize = BenchmarkSuite.sizeBinarySearchTree

  def measureCode(using: super.Using[Int], tdEnum: EnumType) = {
    using in { (size: Int) =>
      val enum = tdEnum.getEnum((size, 1 to size))
      for (i <- 0 until enum.size) enum(i)
//      val list =
//      	for (i <- 0 until enum.size) yield enum(i)
//      println(enum.size == (Catalan.catalan(size)))
//      println(list.size == (Catalan.catalan(size)))
    }
  }

  def warmUp(inEnum: EnumType) {
    for (size <- 1 to maxSize) {
      val enum = inEnum.getEnum((size, 1 to size))
      for (i <- 0 until enum.size) enum(i)
    }
  }

  override def constructEnumerator(implicit ms: MemoizationScope) = {
    Depend.memoized(
      (self: Depend[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        if (size <= 0) e.Singleton(Leaf)
        else if (size == 1)
          e.WrapArray(range map { v => Node(Leaf, v, Leaf) })
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
            memoization.Chain[(Int, Int), (Tree, Tree), Node](rootLeftSizePairs, leftRightPairs,
              (p1: (Int, Int), p2: (Tree, Tree)) => {
                val ((leftSize, currRoot), (leftTree, rightTree)) = (p1, p2)

                Node(leftTree, currRoot, rightTree)
              })

          allNodes
        }
      })
  }

}
