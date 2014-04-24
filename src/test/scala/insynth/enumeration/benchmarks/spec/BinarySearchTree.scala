package insynth
package enumeration
package benchmarks
package spec

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

class BinarySearchTreeBenchmark extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
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
    
    val profileRange = 1 to 15

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
  }

  // paper
//	val roots = Dependent[Range, Int]({ Enum( _: Range ) })
//	val sizes = Dependent[Int, Int]({ Enum( 1 to _ ) })
//	    
//	val bsts: Dependent[(Int, Range), Tree] = Dependent({
//	case (self, (s, r)) => {
//	  if (size <= 0) Enum( Leaf )
//	  else {
//	  val left = {p => (p._1-1, r.start to (p._2-1))} map self
//	  val right = {p => (s-p._1, (p._2+1) to r.end)} map self
//	  
//	  // $ ((ranges otimes sizes) otimes (left otimes right))uparrow_f  $
//	  (roots(range) ** sizes(size)) ** (left ** right) map {
//	    case ((leftSize, currRoot), (leftTree, rightTree)) =>
//	      Node(leftTree, currRoot, rightTree) } }
//	})
  
  
  def constructEnumerator(implicit ms: MemoizationScope) = {
    import e._
    import Enum._

    val rootProducer = Depend(
      (range: Range) => {
        e.WrapArray(range)
      })

    val sizeProducer = Depend(
      (size: Int) => {
        e.WrapArray(0 until size)
      })

    Depend.memoized({
      (self: Depend[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        if (size <= 0) Enum(Leaf)
        else if (size == 1)
          range ↑ { v => Node(Leaf, v, Leaf) }
        else {
          val roots: Finite[Int] = Enum(range)
          val leftSizes: Finite[Int] = Enum(0 until size)

          val rootLeftSizePairs: Finite[(Int, Int)] = leftSizes ⊗ roots

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
      }})
  }

}
