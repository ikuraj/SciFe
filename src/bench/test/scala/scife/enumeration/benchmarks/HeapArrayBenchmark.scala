package scife
package enumeration
package benchmarks

import dependent._
import scife.{ enumeration => e }
import memoization._

import scife.util._
import scife.util.logging._
import structures.BSTrees._

import org.scalatest._
import org.scalameter.api._

import scala.language.postfixOps
import scala.language.existentials

class HeapArrayBenchmark
  extends StructuresBenchmark[Depend[(Int, Range), Tree]]
  with java.io.Serializable {
  import e.Enum

  type EnumType = Depend[(Int, Range), Tree]

  def measureCode(tdEnum: EnumType) = {
    { (size: Int) =>
      val enum = tdEnum.getEnum((size, getRange(size)))
      for (ind <- 0 until enum.size) enum(ind)
    }
  }

  def warmUp(inEnum: EnumType, maxSize: Int) {
    val tdEnum = inEnum.asInstanceOf[EnumType]
    for (size <- 1 to maxSize) {
      val enum = tdEnum.getEnum((size, getRange(size)))
      val elements =
        for (
          ind <- 0 until enum.size
        ) enum(ind)
    }
  }

  def getRange(m: Int) = m to 0 by -1

  def constructEnumerator(implicit ms: MemoizationScope) = {
    Depend.memoized(
      (self: EnumType, pair: (Int, Range)) => {
        // list sorted descendingly
        val (size, range) = pair

        if (size > range.size) e.Empty
        else if (size <= 0) e.Singleton(Leaf)
        else if (size == 1) {
          //        (e.Enum(list): Enum[Int]) map { v => Node(Leaf, v, Leaf) }
          //        e.WrapArray( array map { v => Node(Leaf, v, Leaf) } )
          val arr = new Array[Node](range.size)
          for (i <- 0 until range.size) arr(i) = Node(Leaf, range.start + i, Leaf)
          e.WrapArray(arr)
        } else if (!range.isEmpty) {
          val rootsInds = Enum(range): Finite[Int]

          val leftSize = (size - 1) / 2
          val childHeaps1 = InMap(self, { (rootInd: Int) =>
            (size - 1 - leftSize, getRange(rootInd): Range)
          })
          val childHeaps2 = InMap(self, { (rootInd: Int) =>
            (leftSize, getRange(rootInd): Range)
          })
          val leftRightPairs: Depend[Int, (Tree, Tree)] =
            Product(childHeaps1, childHeaps2)

          val allNodes =
            if (size < 9)
              memoization.Chain[Int, (Tree, Tree), Node](rootsInds, leftRightPairs,
                (rootInd: Int, p2: (Tree, Tree)) => {
                  val (leftTree, rightTree) = p2

                  Node(leftTree, rootInd, rightTree)
                })
            else
              e.dependent.Chain[Int, (Tree, Tree), Node](rootsInds, leftRightPairs,
                (rootInd: Int, p2: (Tree, Tree)) => {
                  val (leftTree, rightTree) = p2

                  Node(leftTree, rootInd, rightTree)
                })

          allNodes
        } else e.Empty
      })
  }

}
