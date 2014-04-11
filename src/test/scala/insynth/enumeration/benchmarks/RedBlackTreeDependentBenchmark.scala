package insynth
package enumeration
package benchmarks

import dependent._
import insynth.{ enumeration => e }
import memoization._

import insynth.util._
import insynth.util.logging._
import Structures.RedBlackTrees._

import org.scalatest._
import org.scalameter.api._

import scala.language.postfixOps
import scala.language.existentials

class RedBlackTreeDependentBenchmark
  extends StructuresBenchmark[Depend[(Int, Range, Set[Boolean], Int), Tree]]
  //extends DependentMemoizedBenchmark[Int, Depend[(Int, Range, Set[Boolean], Int), Tree]]
  with java.io.Serializable with HasLogger {

  override val name = "Red Black Tree"

  val maxSize = BenchmarkSuite.maxSize

  fixture

  type EnumType = Depend[(Int, Range, Set[Boolean], Int), Tree]

  def measureCode(using: super.Using[Int], tdEnum: EnumType) = {
    using in { (size: Int) =>
      val elements =
        for (
          blackHeight <- 0 to (size + 1);
          enum = tdEnum.getEnum(size, 1 to size, Set(true, false), blackHeight);
          ind <- 0 until enum.size
        ) yield enum(ind)
    }
  }

//  def generator = Gen.range("size")(1, maxSize, 1)

  def warmUp(inEnum: EnumType) {
    val tdEnum = inEnum.asInstanceOf[Depend[(Int, Range, Set[Boolean], Int), Tree]]
    for (size <- 1 to maxSize) {
      val tdEnumVal = tdEnum
      val elements =
        for (
          blackHeight <- 0 to (size + 1);
          enum = tdEnum.getEnum(size, 1 to size, Set(true, false), blackHeight);
          ind <- 0 until enum.size
        ) yield enum(ind)
    }
  }

  def constructEnumerator(ms: MemoizationScope) = {
    val rootProducer = Depend(
      (range: Range) => {
        e.WrapArray(range)
      })

    val colorsProducer = Depend(
      (set: Set[Boolean]) => {
        e.WrapArray(set.toIndexedSeq)
      })

    val sizeProducer = Depend(
      (size: Int) => {
        e.WrapArray(0 until size)
      })

    val treesOfSize: Depend[(Int, Range, Set[Boolean], Int), Tree] = Depend.memoized(
      (self: Depend[(Int, Range, Set[Boolean], Int), Tree], pair: (Int, Range, Set[Boolean], Int)) => {
        val (size, range, colors, blackHeight) = pair

        if (size == 0 || range.size < 0 || colors.isEmpty || blackHeight < 0) e.Empty
        if (size == 0 && blackHeight == 1 && colors.contains(true)) e.Singleton(Leaf)
        else if (size == 1) e.WrapArray(range map { v => Node(Leaf, v, Leaf, true) })
        else if (size > 0 && blackHeight >= 1) {
          val roots = rootProducer.getEnum(range)
          val leftSizes = sizeProducer.getEnum(size)
          val rootColors = colorsProducer.getEnum(colors)

          val rootLeftSizePairs = e.Product(leftSizes, roots)
          val rootLeftSizeColorTuples = e.Product(rootLeftSizePairs, rootColors)

          val leftTrees: Depend[((Int, Int), Boolean), Tree] = InMap(self, { (par: ((Int, Int), Boolean)) =>
            val ((leftSize, median), rootColor) = par
            val childColors = if (rootColor) Set(true, false) else Set(true)
            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
            (leftSize, range.start to (median - 1), childColors, childBlackHeight)
          })

          val rightTrees: Depend[((Int, Int), Boolean), Tree] = InMap(self, { (par: ((Int, Int), Boolean)) =>
            val ((leftSize, median), rootColor) = par
            val childColors = if (rootColor) Set(true, false) else Set(true)
            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
            (size - leftSize - 1, (median + 1) to range.end, childColors, childBlackHeight)
          })

          val leftRightPairs: Depend[((Int, Int), Boolean), (Tree, Tree)] =
            Product(leftTrees, rightTrees)

          val allNodes =
            memoization.Chain[((Int, Int), Boolean), (Tree, Tree), Node](rootLeftSizeColorTuples, leftRightPairs,
              (p1: ((Int, Int), Boolean), p2: (Tree, Tree)) => {
                val (((leftSize, currRoot), rootColor), (leftTree, rightTree)) = (p1, p2)

                Node(leftTree, currRoot, rightTree, rootColor)
              })

          allNodes
        } else e.Empty
      })

    treesOfSize
  }

}