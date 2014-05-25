package insynth
package enumeration
package benchmarks
package test

import dependent._
import memoization._
import insynth.{ enumeration => e }
import insynth.util._

import insynth.util.logging._

import Structures._
import BSTrees._

import org.scalatest._
import org.scalatest.prop._
import org.scalameter.api._

import scala.language.existentials

class BinarySearchTreeBenchmarkTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
	HasLogger with ProfileLogger {  
  import common._
  import Util.CheckerHelper
  import Checks._
  
  test("correctness") {
    val ms = new MemoizationScope
    val enum = constructEnumerator(ms)
    ms.memoizations.size should be (1)
    
    val helper = new CheckerHelper[Tree]
    import helper._
    
    withLazyClue("Elements are: " + clue) {
      res = enum.getEnum(1, 1 to 3)
      ms.memoizations.size should be (1)
      res.size should be (3)
      elements should contain theSameElementsAs (1 to 3).map(
        Node(Leaf, _, Leaf)
      )

      res = enum.getEnum(2, 1 to 2)
      ms.memoizations.size should be (2)
      res.size should be (2)
      elements should contain allOf (
        Node(Leaf, 1, Node(Leaf, 2, Leaf)),
        Node(Node(Leaf, 1, Leaf), 2, Leaf)
      )

      res = enum.getEnum(3, 1 to 3)
      ms.memoizations.size should be (7)
      res.size should be (5)
      elements should contain allOf (
        Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
        Node(Leaf, 1, Node(Node(Leaf, 2, Leaf), 3, Leaf))
      )

      res = enum.getEnum(3, 1 to 4)
      ms.memoizations.size should be (12)
      res.size should be (5 * Binomial.binomialCoefficient(4, 3))
      elements should contain allOf (
        Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
        Node(Leaf, 1, Node(Node(Leaf, 2, Leaf), 3, Leaf))
      )

      for (size <- 1 to 3) {
        res = enum.getEnum((size, Range(size, size - 1)))
        res.size should be (0)
        elements should be ('empty)
        
        res = enum.getEnum((0, 1 to size))
        res(0) should be (Leaf)
        res.size should be (1)
      }
    
      // some confirmed counts
//      res = enum.getEnum(12, 1 to 12)
//      res.size should be (208012)
      
    }


    val profileRange = 1 to 10

    for (size <- profileRange) {
      ms.clear
      profile("Getting stream for BST of size %d".format(size)) {
        res = enum.getEnum(size, 1 to size)
      }
      profile("Claculating size for BST of size %d".format(size)) {
        res.size should be (Catalan.catalan(size))
      }
      profile("Getting elements for BST of size %d".format(size)) {
        for (ind <- 0 until res.size) res(ind)
      }
      
      assert( (for (ind <- 0 until res.size) yield res(ind)).forall( invariant(_) ) )
    }
  }

  def constructEnumerator(implicit ms: MemoizationScope) = {
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
