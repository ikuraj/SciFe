package scife
package enumeration
package lazytraversal

import scife.enumeration.dependent._
import memoization._
import scife.{ enumeration => e }
import scife.util._

import scife.util.logging._

import scife.util.structures._
import LazyBSTrees._

import benchmarks._

import org.scalatest._
import org.scalameter.api._

import scala.language.existentials

class BinarySearchTreeNormal2
  extends DependentMemoizedBenchmark[(Int, Int), Depend[(Int, Range), Tree]] //  extends PerformanceTest.OfflineReport with ProfileLogger
  {

  type EType = Depend[(Int, Range), Tree]

  implicit val treeTag = implicitly[reflect.ClassTag[scife.util.structures.LazyBSTrees.Tree]]

  override def generator(maxSize: Int): Gen[(Int, Int)] =
    for (size <- Gen.range("size")(1, maxSize, 1);
      missingEl <- Gen.range("missingElement")(0, size, 1)) yield
      (size, missingEl)
      
  def measureCode(tdEnum: EType) = {
//    { (size: Int) =>
    { (in: (Int, Int)) =>
      val (size, el) = in
//      for (el <- 1 to size) {
//        val enum = tdEnum.getEnum((size - 1, 1 to size - 1))
        val enum = tdEnum.getEnum((size, 1 to size))
        for (i <- 0 until enum.size) {
          val t = enum(i)
          val index = t insert el
          t.lazyInvariant
        }
//      }
    }
  }

  def warmUp(inEnum: EType, maxSize: Int) {
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
            e.dependent.Product(leftTrees, rightTrees)

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
