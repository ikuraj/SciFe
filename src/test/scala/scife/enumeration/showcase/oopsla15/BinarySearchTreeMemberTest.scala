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

import scala.language.postfixOps

class BinarySearchTreeMemberTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {

  // import DSL
  import Enum._
  import Depend._
  import member._
  import member.dependent._

  test("Binary search tree, membership recognition") {
   
  import scife.util.structures.bst.ybanez._ 
  import BST._
    import BinarySearchTreeMemberTest._
 
  type BenchmarkInput = (Int, Int)
  type Input = (Int, List[Int])
  type EnumType = MemberDependFinite[Input, Tree]
    
    val bst =
    new WrapFunctionFin(
      (self: EnumType, pair: (Int, List[Int])) => {
        val (size, range) = pair

        if (size <= 0) new Singleton(Leaf): MemberFinite[Tree]
//        else if (size == 1) new WrapArray(Array(range map { i => Node(Leaf, values(i-1), Leaf) }: _*)): MemberFinite[Tree]
        else {
          val roots = new WrapRange(0 until range.size)
          val leftSizes = new WrapRange(0 until size)

          val rootLeftSizePairs = new member.ProductFinite(leftSizes, roots)

          val leftTrees = new InMapFin(self, { (par: (Int, Int)) =>
            val (leftSize, median) = par
            (leftSize, range.take(median))
          })

          val rightTrees =
            new InMapFin(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (size - leftSize - 1, range.drop(median+1))
            })

          val leftRightPairs =
            //new ProductFinite(leftTrees, rightTrees)
            dependent.Product(leftTrees, rightTrees)

          val allNodes =
            new eager.ChainFinite(rootLeftSizePairs, leftRightPairs): MemberFinite[((Int, Int), (Tree, Tree))]

          val makeTree =
            (p: ((Int, Int), (Tree, Tree))) => {
              val ((leftSize, currRoot), (leftTree, rightTree)) = p

              Node(leftTree, range(currRoot), rightTree)
            }

          val memberTree =
            (t: Tree) => {
              val n = t.asInstanceOf[Node]
              val leftSize = n.left.size
              val currRoot = range.indexOf(n.elem)
              val leftTree = n.left
              val rightTree = n.right

              ((leftSize, currRoot), (leftTree, rightTree))
            }

          new {
            override val classTagT = implicitly[scala.reflect.ClassTag[Tree]]
          } with Map[((Int, Int), (Tree, Tree)), Tree](allNodes, makeTree, memberTree) with
            MemberFinite[Tree] with e.memoization.MemoizedSize with
            e.memoization.MemoizedStatic[Tree] with member.memoization.Memoized[Tree]: MemberFinite[Tree]
        }
      }) with e.dependent.DependFinite[Input, Tree] with e.memoization.dependent.Memoized[Input, Tree]
  
  val bstVerify = bst(5, 1 to 5 toList)
  for (x ← 1 to 5; tree ← bst(4, 1 to 5 toList)) {
    val newT = tree + x
     // insertion without contracts
    assert(newT == tree || bstVerify.member(newT), s"Failed for key=$x and tree $tree (got new tree $newT)") }

      
    // check equality of enumerator up to some size (not too big, since there enumerators do not memoize)
    for (i <- 1 to 5) {
      bst(i, 1 to i toList).size shouldBe bstReference(i, 1 to i).size

      bst(i, 1 to i toList).toList should contain theSameElementsAs bstReference(i, 1 to i).toList
    }
      

  }

}

object BinarySearchTreeMemberTest {
  
  import scife.util.structures.bst.ybanez._
  import BST._
  type Tree = BST[Int]
  type Node = NonEmptyBST[Int]
  val Leaf = EmptyBST 
  
  import Enum._
  import Depend._
  import member._
  import member.dependent._
  
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
