package scife
package enumeration
package benchmarks
package test

import dependent._
import memoization._
import scife.{ enumeration => e }
import scife.util._

import scife.util.logging._

import Structures._
import TreeShapes._

import org.scalatest._
import org.scalatest.prop._
import org.scalameter.api._

import scala.language.existentials
import scala.language.postfixOps

class BinarySearchTreeCompBenchmarkTest extends FunSuite
  with Matchers with GeneratorDrivenPropertyChecks with
  HasLogger with ProfileLogger {
    import Util.CheckerHelper
  import Checks._
  import Math._

  type Output = Tree//(List[Int], Tree)

  test("correctness") {
    val ms = new scope.AccumulatingScope
    val enum = constructEnumerator(ms)
//    ms.memoizations.size should be (2)

    val helper = new CheckerHelper[Output]
    import helper._

    withLazyClue("Elements are: " + clue) {
      res = enum.getEnum(0)
//      ms.memoizations.size should be (2)
      res.size should be (1)
//      elements should contain (
//        (Nil, Leaf)
//      )

      res = enum.getEnum(1)
//      ms.memoizations.size should be (2)
      res.size should be (1)
//      elements should contain (
//        (List(1), Node(Leaf, Leaf))
//      )

      // some confirmed counts
      res = enum.getEnum(7)
      res.size should be (429)
      res = enum.getEnum(12)
      res.size should be (208012)

    }


    val profileRange = 1 to 10

    for (size <- profileRange) {
      ms.clear
      profile("Getting stream for BST of size %d".format(size)) {
        res = enum.getEnum(size)
      }
      profile("Claculating size for BST of size %d".format(size)) {
        res.size should be (Catalan.catalan(size))
      }
      profile("Getting elements for BST of size %d".format(size)) {
        for (ind <- 0 until res.size) res(ind)
      }

    }
  }

  def constructEnumerator(implicit ms: MemoizationScope) = {
//    e.dependent.Product(
//      InMap( constructElements(ms), (s: Int) => (s, s) ),
//      constructTree(ms)
//    )
    constructTree(ms)
  }

  def constructElements(implicit ms: MemoizationScope) = {
    Depend.memoizedFin(
      (self: DependFinite[(Int, Int), List[Int]], pair: (Int, Int)) => {
        val (size, max) = pair

        if (size <= 0) e.Singleton(Nil): Finite[List[Int]]
        else if (size == 1)
          e.WrapArray( (1 to max) map { List(_) } toArray ): Finite[List[Int]]
        else {
          val roots = e.Enum(1 to max)

          val tails: Depend[Int, List[Int]] =
            InMap(self, { (max: Int) =>
              (size - 1, max)
            })

          val allLists =
            memoization.Chain(roots, tails,
              (h: Int, tail: List[Int]) => {
                h :: tail
              })

          allLists
        }
      })
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
