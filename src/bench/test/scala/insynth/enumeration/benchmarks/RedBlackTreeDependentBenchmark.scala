package insynth
package enumeration
package benchmarks

import dependent._
import insynth.{ enumeration => e }
import memoization._

import insynth.util._
import insynth.util.logging._
import structures.RedBlackTrees._

import org.scalatest._
import org.scalameter.api._

import scala.language.postfixOps
import scala.language.existentials

class RedBlackTreeDependentBenchmark
  extends StructuresBenchmark[Depend[(Int, Range, Set[Boolean], Int), Tree]]
  with java.io.Serializable with HasLogger {
  
  type EnumType = Depend[(Int, Range, Set[Boolean], Int), Tree]

  def measureCode(tdEnum: EnumType) = {
    { (size: Int) =>
      for (
        blackHeight <- 1 to (Math.log2(size + 1).toInt + 1);
        enum = tdEnum.getEnum(size, 1 to size, Set(true, false), blackHeight);
        ind <- 0 until enum.size
      ) enum(ind)
    }
  }

  def warmUp(inEnum: EnumType, maxSize: Int) {
    val tdEnum = inEnum.asInstanceOf[Depend[(Int, Range, Set[Boolean], Int), Tree]]
    for (size <- 1 to maxSize) {
      val tdEnumVal = tdEnum
      for (
        blackHeight <- 1 to (Math.log2(size + 1).toInt + 1);
        enum = tdEnum.getEnum(size, 1 to size, Set(true, false), blackHeight);
        ind <- 0 until enum.size
      ) enum(ind)
    }
  }

  def constructEnumerator(implicit ms: MemoizationScope) = {
    val colorsProducer = Depend.memoized(
      (set: Set[Boolean]) => { e.WrapArray(set.toArray) })
    
    val treesOfSize: Depend[(Int, Range, Set[Boolean], Int), Tree] = Depend.memoized(
      (self: Depend[(Int, Range, Set[Boolean], Int), Tree], pair: (Int, Range, Set[Boolean], Int)) => {
        val (size, range, colors, blackHeight) = pair

          if (range.size >= size && range.size < 0 || blackHeight < 0) e.Empty
          else
          if (size == 0 && blackHeight == 1 && colors.contains(true)) e.Singleton(Leaf)
//            else if (size == 1 && blackHeight == 1 && colors.contains(false)) e.WrapArray(range map { v => Node(Leaf, v, Leaf, false) })
//            else if (size == 1 && blackHeight == 2 && colors.contains(true)) e.WrapArray(range map { v => Node(Leaf, v, Leaf, true) })
//            else if (size == 1) e.WrapArray(range map { v => Node(Leaf, v, Leaf, true) })
          else if (size > 0 && blackHeight >= 1) {
            val roots = e.Enum(range)
            val leftSizes = e.WrapArray(0 until size)
            val rootColors = colorsProducer(colors)
    
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