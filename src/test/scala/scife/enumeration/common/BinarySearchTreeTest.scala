package scife
package enumeration
package common

import dependent._
import memoization._
import scife.{ enumeration => e }
import scife.util._

import scife.util.logging._

import structures._
import BSTrees._

import org.scalatest._
import org.scalatest.prop._
import org.scalameter.api._

import scala.language.existentials

object BinarySearchTreeTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
	HasLogger with ProfileLogger {  

  import Util.CheckerHelper
  import Checks._
  import Math._
  
  def testCorrectness(enum: Depend[(Int, Range), Tree]) {
    val helper = new CheckerHelper[Tree]
    import helper._
    
    withLazyClue("Elements are: " + clue) {
      res = enum.getEnum(1, 1 to 3)
      res.size should be (3)
      elements should contain theSameElementsAs (1 to 3).map(
        Node(Leaf, _, Leaf)
      )

      res = enum.getEnum(2, 1 to 2)
      res.size should be (2)
      elements should contain allOf (
        Node(Leaf, 1, Node(Leaf, 2, Leaf)),
        Node(Node(Leaf, 1, Leaf), 2, Leaf)
      )

      res = enum.getEnum(3, 1 to 3)
      res.size should be (5)
      elements should contain allOf (
        Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
        Node(Leaf, 1, Node(Node(Leaf, 2, Leaf), 3, Leaf))
      )

      res = enum.getEnum(3, 1 to 4)
      res.size should be (5 * Binomial.binomialCoefficient(4, 3))
      elements should contain allOf (
        Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
        Node(Leaf, 1, Node(Node(Leaf, 2, Leaf), 3, Leaf))
      )

      for (size <- 1 to 3) {
        res = enum.getEnum((size, Range(size, size - 1)))
        res.size should be (0)
        elements should be ('empty)
        
        res = enum.getEnum((0, 1 to size))
        res(0) should be (Leaf)
        res.size should be (1)
      }
    
    }

    val profileRange = 1 to 5

    for (size <- profileRange) {
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
