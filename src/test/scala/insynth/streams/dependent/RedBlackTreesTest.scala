package insynth
package streams
package dependent

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._

import streams.{ light => e }

import util._
import util.format._
import util.logging._
import common._

class RedBlackTreesTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with Inspectors with
	HasLogger with ProfileLogger {  
  import Checks._
  import Structures._

  import RedBlackTrees._ 
  
  def tests(trees: Dependent[(Int, Range, Set[Boolean], Int), Tree]) {
    var elements: Iterable[Tree] = null
    def clue = elements.mkString("\n")
    
    val profileRange = 9 to 9
    
    withLazyClue("Elements are: " + clue) {
      // specific cases
//      elements = trees.getStream( (3, 1 to 3, Set(true, false), 2) ).toList
//      elements.size should be (2)
//      elements = trees.getStream( (3, 1 to 3, Set(true, false), 3) ).toList
//      elements.size should be (1)
//
//      elements = trees.getStream( (1, 1 to 1, Set(true), 2) ).toList
//      elements.size should be (1)
//      elements = trees.getStream( (2, 1 to 2, Set(true), 2) ).toList
//      elements.size should be (2)
//      elements = trees.getStream( (4, 1 to 4, Set(true, false), 2) ).toList
//      elements.size should be (4)

      for (size <- profileRange) {
        info("Generating for size " + size)
        profile("Getting trees of size %d".format(size)) {
          elements =
            for (blackHeight <- 0 to (size+1); enum = trees.getStream(size, 1 to size, Set(true, false), blackHeight);
              ind <- 0 until enum.size) yield enum(ind)
        }
        
//        forAll( elements ) { invariant(_) should be (true) }
//
//        profile("Claculating size %d".format(size)) {
//          elements.size should be (numberOfTrees(size))
//        }
      }

    }
    
//    trees.getStream(1, 1 to 1, Set(true, false), 1).toList.size should be (1)

    // if size > range.size then no element should be generated
//    forAll ("size", "range max", maxSize(10)) { (s: Int, m: Int) =>
//      whenever (m < s) {
//        trees.getStream(s, 1 to m).size should be (0)
//      }
//    }
    
  }

  test("red black trees") {
    
    val rootProducer = Producer[Range, Int](
      (range: Range) => {
        e.WrapperArray( range )
      }
    )
    
    val colorsProducer = Producer[Set[Boolean], Boolean](
      (set: Set[Boolean]) => {
        e.WrapperArray( set.toIndexedSeq )
      }
    )
    
    val sizeProducer = Producer[Int, Int](
      (size: Int) => {
        e.WrapperArray( 0 until size )
      }
    )
    
    var getTreeOfSize: Dependent[ (Int, Range, Set[Boolean], Int), Tree ] = null
    
    // 
    val treesOfSize: Dependent[ (Int, Range, Set[Boolean], Int), Tree ] = Producer.memoized(
      ( pair: (Int, Range, Set[Boolean], Int) ) => {
        val (size, range, colors, blackHeight) = pair
//        info("got into construction with, size=%d, range=%s, colors=%s, black height=%s".format(size, range, colors, blackHeight))
        assert(size >= 0, "size=%d, range=%s" format (size, range))

//        if (size == 0 || range.size < 0 || colors.isEmpty || blackHeight < 0) e.Empty
        if (size == 0 && blackHeight == 1 && colors.contains(true)) e.Singleton( Leaf )
//        else if (size == 1) e.WrapperArray( range map { v => Node(Leaf, v, Leaf) } )
        else if (size > 0 && blackHeight >= 1) {
          val roots = rootProducer.getStream(range)
          val leftSizes = sizeProducer.getStream(size)
          val rootColors = colorsProducer.getStream(colors)
          
          val rootLeftSizePairs = e.Binary(leftSizes, roots)
          val rootLeftSizeColorTuples = e.Binary(rootLeftSizePairs, rootColors)
          
          val leftTrees: Dependent[((Int, Int), Boolean), Tree] = new InMapper(getTreeOfSize, { (par: ((Int, Int), Boolean)) =>
            val ((leftSize, median), rootColor) = par
            val childColors = if (rootColor) Set(true, false) else Set(true)
            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
            (leftSize, range.start to (median - 1), childColors, childBlackHeight)
          })
          
          val rightTrees: Dependent[((Int, Int), Boolean), Tree] = new InMapper(getTreeOfSize, { (par: ((Int, Int), Boolean)) =>
            val ((leftSize, median), rootColor) = par
            val childColors = if (rootColor) Set(true, false) else Set(true)
            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
            (size - leftSize - 1, (median + 1) to range.end, childColors, childBlackHeight)
          })
          
          val leftRightPairs: Dependent[((Int, Int), Boolean), (Tree, Tree)] =
            CoupledBinary(leftTrees, rightTrees)
          
          import BinaryFiniteMemoized._
          
          val allNodes =
	      		combine[((Int, Int), Boolean), (Tree, Tree), Node](rootLeftSizeColorTuples, leftRightPairs,
//	    		    (leftSize: Int, mid: Int) => (size - 1, range.start to (mid - 1)),
	    		    (p1: ((Int, Int), Boolean), p2: (Tree, Tree)) => {
	    		      val (((leftSize, currRoot), rootColor), (leftTree, rightTree)) = (p1, p2)

		      			assert( ! (size >= 2 && leftSize == 0 && size - leftSize - 1 == 0) )
		      			assert( ! (size >= 2 && leftTree == Leaf && rightTree == Leaf ) )
	    		      assert( !(leftSize > 0 && leftTree == Leaf), "leftSize=%d, leftTree=Leaf".format(leftSize))
	    		      Node( leftTree, currRoot, rightTree, rootColor )
	    		    }
	  		    )
          
  		    allNodes
        } else e.Empty
      }
    )
    
    getTreeOfSize = treesOfSize
    
    tests( treesOfSize )
  }
  
}
