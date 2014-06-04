package insynth.enumeration
package reverse
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
  import Structures._
  import BSTrees._
  
  test("regular enumeration") {
    
    val trees = constructEnumerator
    
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
        
    for (size <- 1 to 5) {
      trees(size, 1 to size).toList should contain theSameElementsAs ( constructEnumeratorNormal(size, 1 to size).toList )
    }

  }
  
  test("reverse enumeration") {
    val trees = constructEnumerator
    
    {
      val en = trees.getEnum(1, 1 to 2): Reverse[Tree]
      for ((revEl, ind) <- List(Node(1), Node(2)).zipWithIndex) {
        en.reverse(revEl) should be (ind)
      }
    }

    forAll(Gen.choose(1, 5), Gen.choose(1, 5), maxSize(50)) { (size: Int, m: Int) =>
      whenever (size >= m) {
        val en = trees.getEnum(size, 1 to m)
        val elements = for (ind <- 0 until en.size) yield en(ind)
        for (ind <- 0 until elements.size) {
          val elToReverse = elements(ind)
          val newInd = en.reverse(elToReverse)
          val newElements = for (innerIn <- newInd until en.size) yield en(innerIn)

          newElements should contain theSameElementsAs (elements.drop(ind))
        }
      }
    }
    
    val normalTrees = constructEnumeratorNormal
    
    forAll(Gen.choose(1, 5), Gen.choose(1, 5), minSuccessful(20)) {
      (size: Int, m: Int) =>
      {
        val normalList = normalTrees.getEnum( size, 1 to m )
        
        for ( ind <- 0 until normalList.size) {
          trees( size, 1 to m ).reverse( normalList(ind) ) should be (ind)
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
      (self: ReverseDependFinite[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        if (size <= 0) new WrapArray( Array(Leaf) ): ReverseFinite[Tree]
        else if (size == 1) new WrapArray( Array(range map { v => Node(Leaf, v, Leaf) }: _* )): ReverseFinite[Tree]
        else {
          val roots = rootProducer.getEnum(range)
          val leftSizes = sizeProducer.getEnum(size)

          val rootLeftSizePairs = new reverse.ProductFinite(leftSizes, roots)

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
            new ChainFinite(rootLeftSizePairs, leftRightPairs): ReverseFinite[((Int, Int), (Tree, Tree))]
            
          val makeTree =
            (p: ((Int, Int), (Tree, Tree)) ) => {
              val ((leftSize, currRoot), (leftTree, rightTree)) = p

              Node(leftTree, currRoot, rightTree)
            }

          val reverseTree =
            (t: Tree) => {
              val n = t.asInstanceOf[Node]
              val leftSize = BSTrees.size(n.l)
              val currRoot = n.v
              val leftTree = n.l
              val rightTree = n.r

              ((leftSize, currRoot), (leftTree, rightTree))
            }
            
          
          new Map[((Int, Int), (Tree, Tree)), Tree]( allNodes, makeTree, reverseTree )
            with ReverseFinite[Tree]
            : ReverseFinite[Tree]
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
