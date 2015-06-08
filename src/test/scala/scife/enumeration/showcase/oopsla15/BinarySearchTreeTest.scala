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

class BinarySearchTreeTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {

  // import DSL
  import e._
  import Enum._
  import Depend._

  test("Binary search tree, different enumerator definitions") {
    import BinarySearchTreeTest._

    val bst =
      rec[(Int, Range), Tree]({
        case (self, (size, r)) => {
          if (size <= 0) Leaf
          else {
            val combinedTrees =
              for (
                m <- r;
                lr = r.start to (m - 1);
                rr = m + 1 to r.end;
                ls <- 0 until size;
                rs = size - ls - 1;
                lt ← self(ls, lr);
                rt ← self(rs, rr)
              ) yield Node(lt, m, rt)

            combinedTrees
          }
        }
      })

    val e = bst(5, 1 to 5) // enumerate all trees of size 15
    for (tree ← e) testFun(tree) // feed into test

    info("Enumerating random trees")
    val rnd = new scala.util.Random(System.currentTimeMillis)
    for (i ← List.fill(10)(rnd.nextInt(e.size))) testFun(e(i)) // 10 trees
    
    val bst2 =
      rec[(Int, Range), Tree]({
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
      bst(i, 1 to i).size shouldBe bst2(i, 1 to i).size

      bst(i, 1 to i).toList should contain theSameElementsAs bst2(i, 1 to i).toList
    }
      
  }

}

object BinarySearchTreeTest {

  // define your own test
  def testFun(tree: Tree) = println(tree.toString)

  // example data structure--full classes of data structures available in scife.util.structures
  trait Tree {
    def invariant(tree: Tree) =
      valueOrdering(tree)

    def valuesInRange(t: Tree, min: Int, max: Int): Boolean = t match {
      case Leaf => true
      case Node(l, v, r) => min <= v && max >= v &&
        valuesInRange(l, min, max) && valuesInRange(r, min, max)
    }
    def valueOrdering(t: Tree): Boolean = {
      def correctOrdering(t: Tree, min: Int, max: Int): Boolean = t match {
        case Leaf => true
        case Node(l, v, r) => min <= v && max > v &&
          correctOrdering(l, min, v) && correctOrdering(r, v + 1, max)
      }

      correctOrdering(t, Int.MinValue, Int.MaxValue)
    }

    def size(t: Tree): Int = t match {
      case Leaf => 0
      case Node(l, v, r) => 1 + size(l) + size(r)
    }
  }

  case object Leaf extends Tree
  case class Node(l: Tree, v: Int, r: Tree) extends Tree {
    def this(v: Int) = this(Leaf, v, Leaf)
  }

  object Node {
    def apply(v: Int) = new Node(v)
  }
}
