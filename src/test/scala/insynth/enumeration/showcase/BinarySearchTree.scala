package insynth
package enumeration
package showcase

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
import scala.language.implicitConversions

class BinarySearchTree extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {
  import common._
  import Util.CheckerHelper
  import Checks._

  // DSL
  import e._
  import Enum._
  import Depend._

  // paper
  //val bst = rec[(Int, Range), Tree]({ case ((size, range), self) ⇒ {
  //if (size == 0) Leaf
  //else {
  //val left: Depend[(Int, Int), Tree] = self ↓
  //{ case (ls, m) ⇒ (ls, range.start to (m−1)) }
  //val right: Depend[(Int, Int), Tree] = self ↓
  //{ case (ls, m) ⇒ (size − ls − 1, (m+1) to range.end) }
  //left ⊗ right ⊘ ((0 until size) ⊗ range) ↑ {
  //case (( , root), (lt, rt)) ⇒ Node(lt, root, rt) }
  //}}})

  test("Correctness of variants of BST enumerator") {
    implicit val ms = new MemoizationScope

    val helper = new CheckerHelper[Tree]
    import helper._

    for (enum <- List(constructEnumeratorAsInPaper,
//      constructEnumeratorSomeAnnotations, 
//      constructEnumerator2,
      constructEnumeratorChainCombinatorRight
    )) {
      withLazyClue("Elements are: " + clue) {
        res = enum.getEnum(1, 1 to 3)
        res.size should be (3)
        elements should contain theSameElementsAs (1 to 3).map(
          Node(Leaf, _, Leaf))

        res = enum.getEnum(2, 1 to 2)
        res.size should be (2)
        elements should contain allOf (
          Node(Leaf, 1, Node(Leaf, 2, Leaf)),
          Node(Node(Leaf, 1, Leaf), 2, Leaf))

        res = enum.getEnum(3, 1 to 3)
        res.size should be (5)
        elements should contain allOf (
          Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
          Node(Leaf, 1, Node(Node(Leaf, 2, Leaf), 3, Leaf)))

        res = enum.getEnum(3, 1 to 4)
        res.size should be (5 * Binomial.binomialCoefficient(4, 3))
        elements should contain allOf (
          Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
          Node(Leaf, 1, Node(Node(Leaf, 2, Leaf), 3, Leaf)))

        for (size <- 1 to 3) {
          res = enum.getEnum((size, size to size - 1))
          res.size should be (0)
          elements should be ('empty)

          res = enum.getEnum((0, 1 to size))
          res(0) should be (Leaf)
          res.size should be (1)
        }

        // some confirmed counts
        res = enum.getEnum(7, 1 to 7)
        res.size should be (429)

      }

      val profileRange = 1 to 8

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

        assert((for (ind <- 0 until res.size) yield res(ind)).forall(invariant(_)))
      }
    }
  }

  def constructEnumeratorChainCombinatorRight(implicit ms: MemoizationScope) = {

    val res =
      rec[(Int, Range), Tree]({
        case (self, (size, r)) => {

          if (size <= 0) Leaf
          else {
            val left: DependFinite[(Int, Int), Tree] =
              self ↓ {
                case (ls, m) =>
                  (ls, r.start to (m - 1))
              }

            val right: DependFinite[(Int, Int), Tree] =
              self ↓ {
                case (ls, m) =>
                  (size - ls - 1, (m + 1) to r.end)
              }

             ((0 until size) ⊗ r) ⊘ (left ⊗ right) ↑ {
              case ((lSize, root), (lTree, rTree)) =>
                Node(lTree, root, rTree)
            }
          }
        }
      })

    res
  }

  def constructEnumeratorAsInPaper(implicit ms: MemoizationScope) = {

    val res =
      rec[(Int, Range), Tree]({
        case (self, (size, r)) => {

          if (size <= 0) Leaf
          else {
            val left: DependFinite[(Int, Int), Tree] =
              self ↓ {
                case (ls, m) =>
                  (ls, r.start to (m - 1))
              }

            val right: DependFinite[(Int, Int), Tree] =
              self ↓ {
                case (ls, m) =>
                  (size - ls - 1, (m + 1) to r.end)
              }

            left ⊗ right ⊘ ((0 until size) ⊗ r) ↑ {
              case ((lSize, root), (lTree, rTree)) =>
                Node(lTree, root, rTree)
            }
          }
        }
      })

    res
  }

  def constructorAsInPaperWithAnnotations(implicit ms: MemoizationScope) = {
    val res =
      rec[(Int, Range), Tree]({
        case (self, (size, r)) => {

          if (size <= 0) Leaf
          else {
            val left: DependFinite[(Int, Int), Tree] =
              self ↓ {
                case (ls, m) =>
                  (ls, r.start to (m - 1))
              }

            val right: DependFinite[(Int, Int), Tree] =
              self ↓ {
                case (ls, m) =>
                  (size - ls - 1, (m + 1) to r.end)
              }

            val x = left ⊗ right
            val y = ( ((0 until size): Finite[Int]) ⊗ (r: Finite[Int]) )
            val z = x ⊘ y

            z ↑ {
              case ((lSize, root), (lTree, rTree)) =>
                Node(lTree, root, rTree)
            }
          }
        }
      })

    res
  }

  def constructEnumerator2(implicit ms: MemoizationScope) = {
    import e._
    import Enum._
    import Depend._

    Depend.memoized({
      (self: Depend[(Int, Range.Inclusive), Tree], pair: (Int, Range)) =>
        {
          val (size, range) = pair

          if (size <= 0) Enum(Leaf)
          else if (size == 1)
            range ↑ { v => Node(Leaf, v, Leaf) }
          else {
            val roots: Finite[Int] = Enum(range)
            val leftSizes: Finite[Int] = Enum(0 until size)

            val rootLeftSizePairs: Finite[(Int, Int)] = leftSizes ⊗ roots

            val leftTrees: Depend[(Int, Int), Tree] =
              self ↓ {
                case (leftSize, median) =>
                  (leftSize, range.start to (median - 1))
              }

            val rightTrees: Depend[(Int, Int), Tree] =
              self ↓ {
                case (leftSize, median) =>
                  (size - leftSize - 1, (median + 1) to range.end)
              }

            val leftRightPairs: Depend[(Int, Int), (Tree, Tree)] =
              //            Product(leftTrees, rightTrees)
              leftTrees ⊗ rightTrees

            val chain: Enum[((Int, Int), (Tree, Tree))] = leftRightPairs ⊘ rootLeftSizePairs

            val allNodes =
              chain ↑ {
                case (p: ((Int, Int), (Tree, Tree))) => {
                  val ((leftSize, currRoot), (leftTree, rightTree)) = p

                  Node(leftTree, currRoot, rightTree)
                }
              }
            //            memoization.Chain[(Int, Int), (Tree, Tree), Node](rootLeftSizePairs, leftRightPairs,
            //              (p1: (Int, Int), p2: (Tree, Tree)) => {
            //                val ((leftSize, currRoot), (leftTree, rightTree)) = (p1, p2)
            //
            //                Node(leftTree, currRoot, rightTree)
            //              })

            allNodes
          }
        }
    })
  }

  def constructEnumeratorSomeAnnotations(implicit ms: MemoizationScope) = {
    import e._
    import Enum._
    import Depend._

    val rootProducer = Depend(
      (range: Range) => {
        e.WrapArray(range)
      })

    val sizeProducer = Depend(
      (size: Int) => {
        e.WrapArray(0 until size)
      })

    Depend.memoized({
      (self: Depend[(Int, Range.Inclusive), Tree], pair: (Int, Range)) =>
        {
          val (size, range) = pair

          if (size <= 0) Enum(Leaf)
          else if (size == 1)
            range ↑ { v => Node(Leaf, v, Leaf) }
          else {
            val roots: Finite[Int] = Enum(range)
            val leftSizes: Finite[Int] = Enum(0 until size)

            val rootLeftSizePairs: Finite[(Int, Int)] = leftSizes ⊗ roots

            val leftTrees: Depend[(Int, Int), Tree] =
              { (par: (Int, Int)) =>
                val (leftSize, median) = par
                (leftSize, range.start to (median - 1))
              } ↓ (self: Depend[(Int, Range.Inclusive), Tree])

            val rightTrees: Depend[(Int, Int), Tree] =
              InMap(self, { (par: (Int, Int)) =>
                val (leftSize, median) = par
                (size - leftSize - 1, (median + 1) to range.end)
              })

            val leftRightPairs: Depend[(Int, Int), (Tree, Tree)] =
              //            Product(leftTrees, rightTrees)
              leftTrees ⊗ rightTrees

            val chain: Enum[((Int, Int), (Tree, Tree))] = leftRightPairs ⊘ rootLeftSizePairs

            val allNodes =
              chain ↑ {
                (p: ((Int, Int), (Tree, Tree))) =>
                  {
                    val ((leftSize, currRoot), (leftTree, rightTree)) = p

                    Node(leftTree, currRoot, rightTree)
                  }
              }
            //            memoization.Chain[(Int, Int), (Tree, Tree), Node](rootLeftSizePairs, leftRightPairs,
            //              (p1: (Int, Int), p2: (Tree, Tree)) => {
            //                val ((leftSize, currRoot), (leftTree, rightTree)) = (p1, p2)
            //
            //                Node(leftTree, currRoot, rightTree)
            //              })

            allNodes
          }
        }
    })
  }

//  def constructRegularNoSize1(implicit ms: MemoizationScope) = {
//    Depend.memoized(
//      (self: Depend[(Int, Range), Tree], pair: (Int, Range)) => {
//        val (size, range) = pair
//
//        if (size <= 0) e.Singleton(Leaf)
//        //        else if (size == 1)
//        //          e.WrapArray(range map { v => Node(Leaf, v, Leaf) })
//        else {
//          val roots = e.Enum(range)
//          val leftSizes = e.Enum(0 until size)
//
//          val rootLeftSizePairs = e.Product(leftSizes, roots)
//
//          val leftTrees: Depend[(Int, Int), Tree] = InMap(self, { (par: (Int, Int)) =>
//            val (leftSize, median) = par
//            (leftSize, range.start to (median - 1))
//          })
//
//          val rightTrees: Depend[(Int, Int), Tree] =
//            InMap(self, { (par: (Int, Int)) =>
//              val (leftSize, median) = par
//              (size - leftSize - 1, (median + 1) to range.end)
//            })
//
//          val leftRightPairs: Depend[(Int, Int), (Tree, Tree)] =
//            Product(leftTrees, rightTrees)
//
//          val allNodes =
//            memoization.Chain[(Int, Int), (Tree, Tree), Node](rootLeftSizePairs, leftRightPairs,
//              (p1: (Int, Int), p2: (Tree, Tree)) => {
//                val ((leftSize, currRoot), (leftTree, rightTree)) = (p1, p2)
//
//                Node(leftTree, currRoot, rightTree)
//              })
//
//          allNodes
//        }
//      })
//  }

}
