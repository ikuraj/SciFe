package scife
package enumeration
package showcase
package oopsla15

import dependent._
import memoization._
import scife.{ enumeration => e }

import scife.util._

import java.util.concurrent._
import scala.collection.JavaConversions._

import org.scalatest._
import org.scalatest.prop._
import org.scalameter.api._

class BinarySearchTreeParallelTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {

  // import DSL
  import e._
  import Enum._
  import Depend._
  import parallel._

  val numThr = Runtime.getRuntime.availableProcessors

  test("Binary search tree, parallel enumeration") {
    import BinarySearchTreeTest._

    val enumeratorFunction =
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
            leftTrees ⊗ rightTrees
            //Product(leftTrees, rightTrees)

          val allNodes =
            e.memoization.Chain[(Int, Int), (Tree, Tree), Node](rootLeftSizePairs, leftRightPairs,
              (p1: (Int, Int), p2: (Tree, Tree)) => {
                val ((leftSize, currRoot), (leftTree, rightTree)) = (p1, p2)

                Node(leftTree, currRoot, rightTree)
              })

          allNodes
        }
      }

    val ms = scope.NoScope
    val bst = Depend.memoizedConcurrentNoScope(enumeratorFunction)
    ms.add(bst)
    
    val treeQueue = new ConcurrentLinkedQueue[Tree]

    def testFun(t: Tree, threadInd: Int) = {
      treeQueue add t
      println(s"Thread $threadInd: enumerated $t") 
    }

    def runner(e: Enum[Tree], b: Int) = new Runnable {
      def run = { for (i ← b until e.size by numThr) testFun(e(i), b) }
    }
    
    val size = 5
    
    val enum = bst.getEnum((size, 1 to size))
    val ex = Executors.newFixedThreadPool(numThr)
    for (t ← 0 until numThr) ex.submit(runner(enum, t)) // start
    ex.shutdown // shutdown and wait for termination
    
    // verify that we enumerated the right trees
    val enumReference = BinarySearchTreeParallelTest.bstReference(size, 1 to size)
    enum.size shouldBe enumReference.size
    treeQueue.size shouldBe enumReference.size
    withClue(s"Difference is ${enumReference.toList.toSet diff treeQueue.toList.toSet}") {
      enumReference.toList.toSet should contain theSameElementsAs treeQueue.toList.toSet
    }
  }

}

object BinarySearchTreeParallelTest {
    import BinarySearchTreeTest._
    
    // import DSL
    import e._
    import Enum._
    import Depend._

    val bstReference =
      Depend.rec[(Int, Range), Tree]({
        case (self, (size, r)) => {
          if (size <= 0) Leaf
          else {
            val left =
              self ↓[(Int, Int)] {
                case (ls, m) =>
                  (ls, r.start to (m - 1))
              }
            val right =
              self ↓[(Int, Int)] {
                case (ls, m) =>
                  (size - ls - 1, (m + 1) to r.end)
              }

            (0 until size) ⊗ r ⊘ (left ⊗ right) ↑ {
              case ((_, root), (lTree, rTree)) =>
                Node(lTree, root, rTree)
            }
          }
        }
      })
}