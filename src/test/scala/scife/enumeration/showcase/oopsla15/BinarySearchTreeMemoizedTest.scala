package scife
package enumeration
package showcase
package oopsla15

import dependent._
import memoization._
import scife.{ enumeration => e }

import scife.util._

import org.scalatest._
import org.scalatest.prop._
import org.scalameter.api._

class BinarySearchTreeMemoizedTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {

  // import DSL
  import e._
  import Enum._
  import Depend._
  
  import dependent._
  import memoization._

  test("Binary search tree, memoization") {
    import BinarySearchTreeTest._

    val memoizationScope = new scope.AccumulatingScope
   
    val time1 = System.currentTimeMillis
    val bst = Depend.memoized(
      (self: Depend[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        if (size <= 0) Singleton(Leaf)
        else {
          val leftTrees =
            self ↓[(Int, Int)] {
              case (ls, m) =>
                (ls, range.start to (m - 1))
            }
          val rightTrees =
            self ↓[(Int, Int)] {
              case (ls, m) =>
                (size - ls - 1, (m + 1) to range.end)
            }
          
          // explicit usage of memoized combinator
          memoization.Chain((0 until size) ⊗ range, leftTrees ⊗ rightTrees,
            (p1: (Int, Int), p2: (Tree, Tree)) => {
              val ((leftSize, currRoot), (leftTree, rightTree)) = (p1, p2)

              Node(leftTree, currRoot, rightTree)
            })
        }
      })
      
    val enum = bst(15, 1 to 15)
    for (i <- enum.size) yield enum(i)
    println(s"Binary search trees of size 15 enumerated in ${System.currentTimeMillis - time1}ms")

    // check whether other definitions of the same enumerator match
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
 
    // check equality of enumerator up to some size (not too big, since there enumerators do not memoize)
    for (i <- 1 to 5) {
      bst(i, 1 to i).size shouldBe bstReference(i, 1 to i).size

      bst(i, 1 to i).toList should contain theSameElementsAs bstReference(i, 1 to i).toList
    }
      
  }

}