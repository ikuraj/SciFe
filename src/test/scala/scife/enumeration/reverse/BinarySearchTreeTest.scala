package scife.enumeration
package reverse

import scife.{ enumeration => e }
import reverse.{ dependent => rd }
import memoization._
import e.dependent._

import util._
import scife.util.logging._
import scife.util._

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._
import org.scalacheck.Gen

import scala.language.existentials

class BinarySearchTreeTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
  HasLogger with ProfileLogger {
  import Checks._
  import Structures._
  import BSTrees._

  def bstTests(trees: Depend[(Int, Range), Tree]) {

    val profileRange = 1 to 5
    val util = new Util.CheckerHelper[Tree]
    import util._

    withLazyClue("Elements are: " + clue) {
      for (size <- profileRange) {
        profile("Getting stream for BST of size %d".format(size)) {
          res = trees.getEnum(size, 1 to size)
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

  }

  private def constructEnumerator(implicit ms: MemoizationScope = null) = {
    val rootProducer = dependent.Reverser(
      (range: Range) => {
        Reverser(range)
      })

    val sizeProducer = dependent.Reverser(
      (size: Int) => {
        Reverser(0 until size)
      })

    rd.Reverser(
      (self: e.dependent.Depend[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        if (size <= 0) Reverser(Leaf)
        else if (size == 1) Reverser(range map { v => Node(Leaf, v, Leaf) })
        else {
          val roots = rootProducer.getEnum(range)
          val leftSizes = sizeProducer.getEnum(size)

          val rootLeftSizePairs = new e.reverse.ProductFinite(leftSizes, roots)

          val leftTrees = rd.InMap(self.asInstanceOf[rd.DependReverse[(Int, Range), Tree]],
          { (par: (Int, Int)) =>
            val (leftSize, median) = par
            (leftSize, range.start to (median - 1))
          })

          val rightTrees =
            rd.InMap(self.asInstanceOf[rd.DependReverse[(Int, Range), Tree]],
            { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (size - leftSize - 1, (median + 1) to range.end)
            })

          val leftRightPairs =
            rd.Product(leftTrees, rightTrees)

          val allNodes =
            new rd.ChainFinite(rootLeftSizePairs, leftRightPairs):
              Reverse[((Int, Int), (Tree, Tree))]

          val makeTree =
            (p: ((Int, Int), (Tree, Tree)) ) => {
              val ((leftSize, currRoot), (leftTree, rightTree)) = p

              Node(leftTree, currRoot, rightTree)
            }

          val reverseTree =
            (n: Node) => {
              val leftSize = BSTrees.size(n.l)
              val currRoot = n.v
              val leftTree = n.l
              val rightTree = n.r

              ((leftSize, currRoot), (leftTree, rightTree))
            }


          new e.reverse.Map[((Int, Int), (Tree, Tree)), Node](
            allNodes, makeTree, reverseTree)
          : Reverse[Tree]
        }
      })
  }

  test("enumerator regular enumeration") {
    bstTests( constructEnumerator )
  }

  test("enumerator reverse enumeration") {
    val lists = constructEnumerator()

    {
      val en = lists.getEnum(1, 1 to 2): Reverse[Tree]
      for ((revEl, ind) <- List(Node(1), Node(2)).zipWithIndex) {
        en.reverse(revEl) should be (ind)
      }
    }

    forAll(Gen.choose(1, 5), Gen.choose(1, 5), maxSize(50)) { (size: Int, m: Int) =>
      whenever (size >= m) {
        val en = lists.getEnum(size, 1 to m)
        val elements = for (ind <- 0 until en.size) yield en(ind)
        for (ind <- 0 until elements.size) {
          val elToReverse = elements(ind)
          val newInd = en.reverse(elToReverse)
          val newElements = for (innerIn <- newInd until en.size) yield en(innerIn)

          newElements should contain theSameElementsAs (elements.drop(ind))
        }
      }
    }
  }

}
