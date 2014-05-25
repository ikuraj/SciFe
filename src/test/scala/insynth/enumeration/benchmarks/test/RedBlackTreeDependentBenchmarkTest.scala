package insynth
package enumeration
package benchmarks
package test

import dependent._
import memoization._
import insynth.{ enumeration => e }
import insynth.util._

import insynth.util.logging._

import Structures.RedBlackTrees._

import org.scalatest._
import org.scalatest.prop._
import org.scalameter.api._

import scala.language.postfixOps
import scala.language.existentials

class RedBlackTreeDependentBenchmarkTest
  extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
  HasLogger with ProfileLogger {
  import common._
  import Util._
  import Checks._
  
  def checker(seq: Seq[Tree]) {
    seq.distinct.size should be (seq.size)
    seq.forall ( blackInv(_) ) should be (true) 
    seq.forall ( redDescHaveBlackChildren(_) ) should be (true) 
    seq.forall ( valueOrdering(_) ) should be (true) 
    seq.forall ( invariant(_) ) should be (true) 
  }
  
  test("correctness") {
    val ms = new MemoizationScope
    val enum = constructEnumerator(ms)
    ms.memoizations.size should be (2)
    
    val helper = new CheckerHelperFun(checker)
    import helper._

    val profileRange = 1 to 10
    
    withLazyClue("Elements are: " + clue) {
      // specific cases
      elements = enum.getEnum( (1, 1 to 1, Set(true), 2) ).toList
      elements.size should be (1)
      elements = enum.getEnum( (2, 1 to 2, Set(true), 2) ).toList
      elements.size should be (2)
      elements = enum.getEnum( (4, 1 to 4, Set(true, false), 2) ).toList
      elements.size should be (4)

      elements = enum.getEnum( (3, 1 to 3, Set(true, false), 2) ).toList
      elements.size should be (2)
      elements = enum.getEnum( (3, 1 to 3, Set(true, false), 3) ).toList
      elements.size should be (1)

      for (size <- profileRange) {
        info("Generating for size " + size)
        elements =
          for (blackHeight <- 0 to (size+1); e = enum.getEnum(size, 1 to size, Set(true, false), blackHeight);
            ind <- 0 until e.size) yield e(ind)
        
        elements.forall( invariant(_) ) should be (true)

        profile("Claculating size %d".format(size)) {
          elements.size should be (numberOfTrees(size))
        }
      }

      // logged
      for (size <- profileRange) {
        info("Generating for size " + size)
        elements =
          for (blackHeight <- 1 to (Math.log2(size + 1).toInt + 1); e = enum.getEnum(size, 1 to size, Set(true, false), blackHeight);
            ind <- 0 until e.size) yield e(ind)
        
        elements.forall( invariant(_) ) should be (true)

        profile("Claculating size %d".format(size)) {
          elements.size should be (numberOfTrees(size))
        }
      }
    
      // some confirmed counts
      elements =
        for (blackHeight <- 0 to 6; e = enum.getEnum(12, 1 to 12, Set(true, false), blackHeight);
          ind <- 0 until e.size) yield e(ind)

      // this is for size 12
      elements.size should be (1296)
    }

    for (size <- profileRange) {
      ms.clear
      profile("Getting enums and elements for RBT of size %d".format(size)) {
        elements =
          for (blackHeight <- 0 to (size+1); e = enum.getEnum(size, 1 to size, Set(true, false), blackHeight);
            ind <- 0 until e.size) yield e(ind)
      }
    }

    for (size <- profileRange) {
      ms.clear
      profile("Getting enums and elements for RBT of size %d".format(size)) {
        elements =
          for (blackHeight <- 1 to (Math.log2(size + 1).toInt + 1); e = enum.getEnum(size, 1 to size, Set(true, false), blackHeight);
            ind <- 0 until e.size) yield e(ind)
      }
      
      assert( (for (el <- elements) yield el).forall( invariant(_) ) )
    }
    
  }

  def constructEnumerator(implicit ms: MemoizationScope) = {
//    val rootProducer = Depend(
//      (range: Range) => {
//        e.WrapArray(range)
//      })

    val colorsProducer = Depend.memoized(
      (set: Set[Boolean]) => { e.WrapArray(set.toArray) })

//    val sizeProducer = Depend(
//      (size: Int) => {
//        e.WrapArray(0 until size)
//      })

    val treesOfSize: Depend[(Int, Range, Set[Boolean], Int), Tree] = Depend.memoized(
      (self: Depend[(Int, Range, Set[Boolean], Int), Tree], pair: (Int, Range, Set[Boolean], Int)) => {
        val (size, range, colors, blackHeight) = pair

        if (range.size >= size && range.size < 0 || blackHeight < 0) e.Empty
        else
        if (size == 0 && blackHeight == 1 && colors.contains(true)) e.Singleton(Leaf)
//        else if (size == 1 && blackHeight == 1 && colors.contains(false)) e.WrapArray(range map { v => Node(Leaf, v, Leaf, false) })
//        else if (size == 1 && blackHeight == 2 && colors.contains(true)) e.WrapArray(range map { v => Node(Leaf, v, Leaf, true) })
//        else if (size == 1) e.WrapArray(range map { v => Node(Leaf, v, Leaf, true) })
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

                assert( ! (size >= 2 && leftSize == 0 && size - leftSize - 1 == 0) )
                assert( ! (size >= 2 && leftTree == Leaf && rightTree == Leaf ) )
                assert( !(leftSize > 0 && leftTree == Leaf), "leftSize=%d, leftTree=Leaf".format(leftSize))
                Node(leftTree, currRoot, rightTree, rootColor)
              })

          allNodes
        } else e.Empty
      })

    treesOfSize
  }

}