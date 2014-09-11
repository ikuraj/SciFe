package insynth.enumeration
package member
package testcase

import insynth.{ enumeration => e }
import dependent._

import util._
import insynth.util.logging._
import insynth.util._

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class BinarySearchTreeTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
	HasLogger with ProfileLogger {  
  import Checks._
  import structures._
  import BSTrees._
  
  test("regular enumeration") {
    
    common.BinarySearchTreeTest.testCorrectness( constructEnumerator )

  }
  
  test("member enumeration") {
    val trees = constructEnumerator
    
    {
      val en = trees.getEnum(1, 1 to 2): Member[Tree]
      for ((revEl, ind) <- List(Node(1), Node(2)).zipWithIndex) {
        en.member(revEl) should be (true)
      }
    }

    val normalTrees = constructEnumeratorNormal
    
    forAll(Gen.choose(1, 5), Gen.choose(1, 5), minSuccessful(20)) {
      (size: Int, m: Int) =>
      {
        val normalList = normalTrees.getEnum( size, 1 to m )
        
        for ( ind <- 0 until normalList.size) {
          trees( size, 1 to m ).member( normalList(ind) ) should be (true)
        }
        
      }
    }
  }

  private def constructEnumerator = {
    val rootProducer = new WrapFunctionFin(
      (range: Range) => { new WrapRange(range) }
    )

    val sizeProducer = new WrapFunctionFin(
      (size: Int) => { new WrapRange (0 until size) }
    )

    new WrapFunctionFin(
      (self: MemberDependFinite[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        if (size <= 0) new WrapArray( Array(Leaf) ): MemberFinite[Tree]
        else if (size == 1) new WrapArray( Array(range map { v => Node(Leaf, v, Leaf) }: _* )): MemberFinite[Tree]
        else {
          val roots = rootProducer.getEnum(range)
          val leftSizes = sizeProducer.getEnum(size)

          val rootLeftSizePairs = new member.ProductFinite(leftSizes, roots)

          val leftTrees = new InMapFin(self, { (par: (Int, Int)) =>
            val (leftSize, median) = par
            (leftSize, range.start to (median - 1))
          })

          val rightTrees =
            new InMapFin(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (size - leftSize - 1, (median + 1) to range.end)
            })

          val leftRightPairs =
            new ProductFinite(leftTrees, rightTrees)

          val allNodes =
            new ChainFinite(rootLeftSizePairs, leftRightPairs): MemberFinite[((Int, Int), (Tree, Tree))]
            
          val makeTree =
            (p: ((Int, Int), (Tree, Tree)) ) => {
              val ((leftSize, currRoot), (leftTree, rightTree)) = p

              Node(leftTree, currRoot, rightTree)
            }

          val memberTree =
            (t: Tree) => {
              val n = t.asInstanceOf[Node]
              val leftSize = BSTrees.size(n.l)
              val currRoot = n.v
              val leftTree = n.l
              val rightTree = n.r

              ((leftSize, currRoot), (leftTree, rightTree))
            }
            
          
          new Map[((Int, Int), (Tree, Tree)), Tree]( allNodes, makeTree, memberTree )
            with MemberFinite[Tree]
            : MemberFinite[Tree]
        }
      })
  }

  private def constructEnumeratorNormal = {
    import e.dependent._

    val rootProducer = new WrapFunctionFin(
      (range: Range) => { new WrapRange(range) }
    )

    val sizeProducer = new WrapFunctionFin(
      (size: Int) => {
        Enum(0 until size)
      })

    Depend.fin(
      (self: e.dependent.DependFinite[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        if (size <= 0) Enum(Leaf): Finite[Tree]
        else if (size == 1) Enum( range.toArray map { v => Node(Leaf, v, Leaf) } ): Finite[Tree]
        else {
          val roots = rootProducer.getEnum(range)
          val leftSizes = sizeProducer.getEnum(size)

          val rootLeftSizePairs = e.Product(leftSizes, roots)

          val leftTrees = InMap(self,
          { (par: (Int, Int)) =>
            val (leftSize, median) = par
            (leftSize, range.start to (median - 1))
          })

          val rightTrees =
            InMap(self,
            { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (size - leftSize - 1, (median + 1) to range.end)
            })

          val leftRightPairs =
            Product(leftTrees, rightTrees)

          val allNodes =
            new ChainFinite(rootLeftSizePairs, leftRightPairs)
        		
          val makeTree =
            (p: ((Int, Int), (Tree, Tree)) ) => {
              val ((leftSize, currRoot), (leftTree, rightTree)) = p

              Node(leftTree, currRoot, rightTree)
            }

          allNodes map makeTree: Finite[Tree]
        }
      })
  }

}
