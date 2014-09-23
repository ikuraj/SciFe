package scife
package enumeration
package common

import dependent._
import memoization._
import scife.{ enumeration => e }
import scife.util._

import scife.util.logging._

import structures.RedBlackTrees._

import org.scalatest._
import org.scalatest.prop._
import org.scalameter.api._

import scala.language.postfixOps
import scala.language.existentials

object RedBlackTreeTest
  extends Matchers with HasLogger with ProfileLogger {
    import Util._
  import Checks._
  
  def assertIndividualInvariants(tree: Tree) {
    ( blackInv(tree) ) should be (true) 
    ( redDescHaveBlackChildren(tree) ) should be (true) 
    ( valueOrdering(tree) ) should be (true) 
    ( invariant(tree) ) should be (true) 
  }
  
  def testCorrectness(enum: Depend[(Int, Range, Set[Boolean], Int), Tree]) {
      
    val helper = new CheckerHelperFun( (_:Seq[Tree]) => true )
    import helper._
    val profileRange = 1 to 5
    
    withLazyClue("Elements are: " + clue) {
      // specific cases
      elements = enum.getEnum( (1, 1 to 1, Set(true), 2) ).toList
      elements.size should be (1)
      elements = enum.getEnum( (2, 1 to 2, Set(true), 2) ).toList
      elements.size should be (2)
      elements = enum.getEnum( (4, 1 to 4, Set(true, false), 2) ).toList
      elements.size should be (4)

      elements = enum.getEnum( (3, 1 to 3, Set(true, false), 2) ).toList
      elements.size should be (2)
      elements = enum.getEnum( (3, 1 to 3, Set(true, false), 3) ).toList
      elements.size should be (1)

      for (size <- profileRange) {
        info("Generating for size " + size)
        elements =
          for (blackHeight <- 0 to (size+1); e = enum.getEnum(size, 1 to size, Set(true, false), blackHeight);
            ind <- 0 until e.size) yield e(ind)
        
        elements.forall( invariant(_) ) should be (true)

        profile("Claculating size %d".format(size)) {
          elements.size should be (numberOfTrees(size))
        }
      }

      // logged
      for (size <- profileRange) {
        info("Generating for size " + size)
        elements =
          for (blackHeight <- 1 to (Math.log2(size + 1).toInt + 1); e = enum.getEnum(size, 1 to size, Set(true, false), blackHeight);
            ind <- 0 until e.size) yield e(ind)
        
        elements.forall( invariant(_) ) should be (true)

        profile("Claculating size %d".format(size)) {
          elements.size should be (numberOfTrees(size))
        }
      }
    
      // some confirmed counts
      elements =
        for (blackHeight <- 0 to 4; e = enum.getEnum(7, 1 to 7, Set(true, false), blackHeight);
          ind <- 0 until e.size) yield e(ind)

      // this is for size 7
      elements.size should be (35)
    }

    for (size <- profileRange) {
      profile("Getting enums and elements for RBT of size %d".format(size)) {
        elements =
          for (blackHeight <- 1 to (Math.log2(size + 1).toInt + 1); e = enum.getEnum(size, 1 to size, Set(true, false), blackHeight);
            ind <- 0 until e.size) yield e(ind)
      }
      
      assert( (for (el <- elements) yield el).forall( invariant(_) ) )
    }
    
  }

}