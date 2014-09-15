package scife
package enumeration
package benchmarks

import dependent._
import memoization._
import scife.{ enumeration => e }
import scife.util._

import scife.util.logging._

import Structures._
import TreeShapes._

import org.scalatest._
import org.scalameter.api._

import scala.language.existentials

class BinarySearchTreeComp
  extends StructuresBenchmark[Depend[Int, Tree]]
//  extends PerformanceTest.OfflineReport with HasLogger with ProfileLogger
  {

  type EnumType = Depend[Int, Tree]

  def measureCode(tdEnum: EnumType) = {
    { (size: Int) =>
      val enum = tdEnum.getEnum((size))
      for (i <- 0 until enum.size) enum(i)
    }
  }

  def warmUp(inEnum: EnumType, maxSize: Int) {
    for (size <- 1 to maxSize) {
      val enum = inEnum.getEnum((size))
      for (i <- 0 until enum.size) enum(i)
    }
  }

  def constructEnumerator(implicit ms: MemoizationScope) = {
    constructTree(ms)
  }

  def constructTree(implicit ms: MemoizationScope) = {
    Depend.memoized(
      (self: Depend[Int, Tree], size: Int) => {

        if (size <= 0) e.Singleton(Leaf)
        else if (size == 1)
          e.Singleton( Node(Leaf, Leaf) )
        else {
          val leftSizes = e.Enum(0 until size)

          val rightTrees: Depend[Int, Tree] =
            InMap(self, { (leftSize: Int) =>
              (size - leftSize - 1)
            })

          val leftRightPairs: Depend[Int, (Tree, Tree)] =
            Product(self, rightTrees)

          val allNodes =
            memoization.Chain[Int, (Tree, Tree), Node](leftSizes, leftRightPairs,
              (p1: Int, p2: (Tree, Tree)) => {
                val (leftTree, rightTree) = p2

                Node(leftTree, rightTree)
              })

          allNodes
        }
      })
  }

}
