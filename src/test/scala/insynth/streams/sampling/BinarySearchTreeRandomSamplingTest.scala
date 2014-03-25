package insynth
package streams
package light
package sampling

import streams.{ light => e }
import streams.{ dependent => d }

import util._
import util.format._
import util.logging._
import common._

import scala.util.Random

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._

class BinarySearchTreeRandomSamplingTest extends FunSuite	with
	Matchers with GeneratorDrivenPropertyChecks with
	HasLogger with ProfileLogger {  
  import Checks._
  import Structures._
  import BSTrees._
  
  implicit val memScope = new MemoizationScope
  
  test ("random sampling") {
    val profileRange = 10 to 10
    val sampleSizes = 5 to 100 by 5
    val tdEnum = produceEnum
    
    val rnd = new Random(System.currentTimeMillis)
    
    for (size <- profileRange) {
      val enum = 
        profile("Getting stream for BST of size %d".format(size)) {
          tdEnum(size, 1 to size)
        }
      profile("Claculating size for BST of size %d".format(size)) {
        enum.size should be (Catalan.catalan(size))
      }
      for (sample <- sampleSizes) {
        memScope.clear
        val sampleSize = enum.size * sample / 100
        val randoms = for (ind <- 0 until sampleSize) yield rnd.nextInt(enum.size)
        profile("Getting elements for BST of sample size %d".format(sampleSize)) {
          for (ind <- randoms) enum(ind)
        }
      }
      
      assert( (for (ind <- 0 until enum.size) yield enum(ind)).forall( invariant(_) ) )
    }
    
  }
  
  import e._

  def produceEnum = {
    val ranges: Dependent[Range, Int] = Producer( Samplable( _: Range ) )
    val sizes = Producer( (size: Int) => ranges.getStream( 0 until size ) )
    
    Producer.memoized(
      (self: Dependent[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair
    		info("(size, range)=" + pair)

        if (size <= 0) Samplable(Leaf)
        else if (size == 1) Samplable(range map { v => Node(Leaf, v, Leaf) })
        else {
          val roots = ranges.getStream(range)
          val leftSizes = sizes.getStream(size)

          val rootLeftSizePairs = new e.BinaryFinite(leftSizes, roots) with SamplableEnum[(Int, Int)]

          val leftTrees: Dependent[(Int, Int), Tree] = new d.InMapper(self, { (par: (Int, Int)) =>
            val (leftSize, median) = par
            (leftSize, range.start to (median - 1))
          })

          val rightTrees: Dependent[(Int, Int), Tree] =
            new InMapper(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (size - leftSize - 1, (median + 1) to range.end)
            })

          val leftRightPairs: Dependent[(Int, Int), (Tree, Tree)] =
            CoupledBinary(leftTrees, rightTrees)

          import BinaryFiniteMemoized._

          val allNodes =
            combine[(Int, Int), (Tree, Tree), Node](rootLeftSizePairs, leftRightPairs,
              (p1: (Int, Int), p2: (Tree, Tree)) => {
                val ((leftSize, currRoot), (leftTree, rightTree)) = (p1, p2)

                Node(leftTree, currRoot, rightTree)
              })

          allNodes
        }
      })
  }
  
}
