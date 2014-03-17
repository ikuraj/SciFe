package insynth
package streams
package dependent

import org.scalatest._
import org.scalatest.matchers._

import streams.{ light => e }

import util._
import util.format._
import util.logging._
import common._

class StreamCombinationTest extends FunSuite with Matchers with
	HasLogger with ProfileLogger {  
  import Checks._

//  test("sorted lists") {
//    
//    val maxLength = 10
//    
//    val intProducer = Producer.finite(
//      { (v: Int) => e.Singleton(v) }
//    )
//    
//    var getList: Int => e.Finite[List[Int]] = null
//    
//    val listChooser: FiniteDependent[Int, List[Int]] = Producer.finite(
//       { (v: Int) =>
//        v match {
//          case 0 => e.Singleton(List[Int]())
//          case _ => e.Binary( e.Singleton(v), getList(v - 1) )( _ :: _ )
//        }
//       }
//    )
//    
//    getList = (v: Int) => listChooser.getStream(v)
//    
//    val enum = listChooser.getStream(5)
//    
//    enum.size should be (1)
//  }

//  test("funny lists") {
//    
//    val maxLength = 10
//    
//    val endElementProducer = Producer[(List[Int], Range), List[Int]](
//      (pair: (List[Int], Range)) => {
//        val (list, range) = pair
//        val sum = list.sum
//        if (sum > range.end) e.Empty
//        else e.WrapperArray( sum to range.end map { _ :: list } )
//      }
//    )
//
//    var getList: ( (Int, Range) ) => e.Enum[List[Int]] = null
//    
//    val listDep: Dependent[ (Int, Range), List[Int] ] = Producer(
//      ( pair: (Int, Range) ) => {
//        val (size, range) = pair
//        if (size <= 0) e.Singleton( List() )
//        else if (size == 1) e.WrapperArray( range map { List(_) } )
//        else {
//          val smallerList = getList( (size-1, range) )
//          BinaryFinite.chain(smallerList, endElementProducer)( (x: List[Int]) => (x, range) )
//        }
//      }
//    )
//    
//    getList = (v: (Int, Range)) => listDep.getStream(v)
//    
//    var res: light.Enum[List[Int]] = null
//    def clue = (0 until res.size).map(res(_)).mkString(",") 
//    
//    withLazyClue("Elements are: " + clue) {
//      res = listDep.getStream((0, 1 to 3))
//      res(0) should be (Nil)
//      res.size should be (1)
//      
//      res = listDep.getStream((1, 1 to 3))
//      res.size should be (3)
//      (0 until res.size).map(res(_)) should contain allOf ( List(1), List(2), List(3) )
//      
//      (1 to 3).end should be (3)
//      res = listDep.getStream( (3, (1 to 3)) )
//      res.size should be (3)
//      var elements = (0 until res.size) map { res(_) }
//      elements should contain allOf (List(2, 1, 1), List(3, 1, 1), List(3, 2, 1))
//
//      res = listDep.getStream( (4, (1 to 5)) )
//      res.size should be (3)
//      elements = (0 until res.size) map { res(_) }
//      elements should contain allOf (
//        List(4, 2, 1, 1), List(5, 2, 1, 1),
//        List(5, 3, 1, 1)
//      )
//
//      res = listDep.getStream( (5, (1 to 10)) )
//      elements = (0 until res.size) map { res(_) }
//      elements should contain only (
//        List(8, 4, 2, 1, 1), List(9, 5, 2, 1, 1), List(10, 5, 2, 1, 1),
//        List(10, 6, 2, 1, 1), List(10, 5, 3, 1, 1),
//        List(9, 4, 2, 1, 1), List(10, 4, 2, 1, 1)
//      )
//    }
//  }
//
//  test("binary search trees") {
//    import Structures._
//    import BSTrees._
//    
//    val maxLength = 10
//    
//    val rootProducer = Producer[Range, Int](
//      (range: Range) => {
//        e.WrapperArray( range )
//      }
//    )
//    
//    val sizeProducer = Producer[Int, Int](
//      (size: Int) => {
//        e.WrapperArray( 0 to size )
//      }
//    )
//    
//    var getTreeOfSize: Dependent[ (Int, Range), Tree ] = null
//    var getTreeOfSizes: Dependent[ (Int, Range), (Tree, Int) ] = null
//
//    // for a given range, and sizes 0..size
//    val treesUpToSize: Dependent[ (Int, Range), (Tree, Int) ] = Producer(
//      ( pair: (Int, Range) ) => {
//        val (size, range) = pair
//        
//        // range is fixed, size is not
//        val sizes = sizeProducer.getStream(size)
//
//        BinaryFinite.chainCombined(sizes, getTreeOfSize,
//          (s: Int) => (s, range), (currSize: Int, t: Tree) => (t, currSize))
//      }
//    )
//    
//    val treesOfSize: Dependent[ (Int, Range), Tree ] = Producer(
//      ( pair: (Int, Range) ) => {
//        val (size, range) = pair
//        assert(size >= 0)
//        // do not care about the range, size is important (rangeProduced can return Empty)
//        if (size <= 0/* || range.isEmpty*/) e.Singleton( Leaf )
//        else if (size == 1) e.WrapperArray( range map { v => Node(Leaf, v, Leaf) } )
//        else {
//          val roots = rootProducer.getStream(range)
//          
//          val leftTrees =
//	      		BinaryFinite.chainCombined(roots, getTreeOfSizes,
//	    		    (mid: Int) => (size - 1, range.start to (mid - 1)),
//	    		    (currRoot: Int, leftPair: (Tree, Int)) => {
//	    		      val (leftTree: Tree, leftSize: Int) = leftPair
//	    		      assert( !(leftSize > 0 && leftTree == Leaf), "leftSize=%d, leftTree=Leaf".format(leftSize))
//	    		      ( leftTree, leftSize, currRoot )
//	    		    }
//	  		    )
//          
//      		BinaryFinite.chainCombined(leftTrees, getTreeOfSize,
//    		    (tuple: (Tree, Int, Int)) => {
//    		      val (_, leftSize, root) = tuple
//    		      assert( ! (size >= 2 && leftSize == 0 && size - leftSize - 1 == 0) )
//    		      (size - leftSize - 1, (root + 1) to range.end)
//    		    },
//    		    (leftTuple: (Tree, Int, Int), rightTree: Tree) => {
//    		      val (leftTree, _, root) = leftTuple
//    		      
//    		      assert( ! (size >= 2 && leftTree == Leaf && rightTree == Leaf ),
//  		          "leftSize=%d (Leaf), rightSize=%d (Leaf)"
//    		          .format(leftTuple._2, size - leftTuple._2 - 1) )
//    		      Node(leftTree, root, rightTree)
//    		    }
//  		    )
//        }
//      }
//    )
//    
//    getTreeOfSize = treesOfSize
//    getTreeOfSizes = treesUpToSize
//    
//    val trees = treesOfSize
//    
//    object OMG {
//	    var _res: light.Enum[Tree] = null
//	    var elements: Seq[Tree] = null
//	    def res = _res
//	    def res_=(n: light.Enum[Tree]) = {
//	      _res = n
//	  		elements = (0 until res.size) map { res(_) }
//	    }
//	    def clue = (0 until res.size).map(res(_)).mkString(",")
//    }
//    import OMG._
//    
//    withLazyClue("Elements are: " + clue) {
//      for (size <- 1 to 3) {
//	      res = trees.getStream((size, Range(size, size - 1)))
//	      res.size should be (0)
//	      elements should be ('empty)
//	      
//	      res = trees.getStream((0, 1 to size))
//	      res(0) should be (Leaf)
//	      res.size should be (1)
//      }
//
//      res = trees.getStream(1, 1 to 3)
//      res.size should be (3)
//      elements should contain theSameElementsAs (1 to 3).map(
//    		Node(Leaf, _, Leaf)
//      )
//
//      res = trees.getStream(2, 1 to 2)
//      res.size should be (2)
//      elements should contain allOf (
//    		Node(Leaf, 1, Node(Leaf, 2, Leaf)),
//    		Node(Node(Leaf, 1, Leaf), 2, Leaf)
//      )
//
//      res = trees.getStream(3, 1 to 3)
//      res.size should be (5)
//      elements should contain allOf (
//    		Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
//    		Node(Leaf, 1, Node(Node(Leaf, 2, Leaf), 3, Leaf))
//      )
//
//      res = trees.getStream(3, 1 to 4)
//      res.size should be (5 * Binomial.binomialCoefficient(4, 3))
//      elements should contain allOf (
//    		Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
//    		Node(Leaf, 1, Node(Node(Leaf, 2, Leaf), 3, Leaf))
//      )
//
//      for (size <- 10 to 10) {
//	      profile("Getting stream for BST of size %d".format(size)) {
//	      	res = trees.getStream(size, 1 to size)
//	      }
//	      profile("Claculating size for BST of size %d".format(size)) {
//	      	res.size should be (Catalan.catalan(size))
//	      }
//	      profile("Getting elements for BST of size %d".format(size)) {
//		      for (ind <- 0 until res.size) res(ind)
//	      }
//      }
//      
//    }
//  }

  test("binary search trees, memoized") {
    import Structures._
    import BSTrees._
    
    val maxLength = 10
    
    val rootProducer = Producer[Range, Int](
      (range: Range) => {
        e.WrapperArray( range )
      }
    )
    
    val sizeProducer = Producer[Int, Int](
      (size: Int) => {
        e.WrapperArray( 0 to size )
      }
    )
    
    var getTreeOfSize: Dependent[ (Int, Range), Tree ] = null
    var getTreeOfSizes: Dependent[ (Int, Range), (Tree, Int) ] = null
    
    import BinaryFiniteMemoized._

    // for a given range, and sizes 0..size
    val treesUpToSize: Dependent[ (Int, Range), (Tree, Int) ] = Producer.memoized(
      ( pair: (Int, Range) ) => {
        val (size, range) = pair
        
        // range is fixed, size is not
        val sizes = sizeProducer.getStream(size)

        chainCombined(sizes, getTreeOfSize,
          (s: Int) => (s, range), (currSize: Int, t: Tree) => (t, currSize))
      }
    )
    
    val treesOfSize: Dependent[ (Int, Range), Tree ] = Producer.memoized(
      ( pair: (Int, Range) ) => {
        val (size, range) = pair
        assert(size >= 0)
        // do not care about the range, size is important (rangeProduced can return Empty)
        if (size <= 0/* || range.isEmpty*/) e.Singleton( Leaf )
        else if (size == 1) e.WrapperArray( range map { v => Node(Leaf, v, Leaf) } )
        else {
          val roots = rootProducer.getStream(range)
          
          val leftTrees =
	      		chainCombined(roots, getTreeOfSizes,
	    		    (mid: Int) => (size - 1, range.start to (mid - 1)),
	    		    (currRoot: Int, leftPair: (Tree, Int)) => {
	    		      val (leftTree: Tree, leftSize: Int) = leftPair
	    		      assert( !(leftSize > 0 && leftTree == Leaf), "leftSize=%d, leftTree=Leaf".format(leftSize))
	    		      ( leftTree, leftSize, currRoot )
	    		    }
	  		    )
          
      		chainCombined(leftTrees, getTreeOfSize,
    		    (tuple: (Tree, Int, Int)) => {
    		      val (_, leftSize, root) = tuple
    		      assert( ! (size >= 2 && leftSize == 0 && size - leftSize - 1 == 0) )
    		      (size - leftSize - 1, (root + 1) to range.end)
    		    },
    		    (leftTuple: (Tree, Int, Int), rightTree: Tree) => {
    		      val (leftTree, _, root) = leftTuple
    		      
    		      assert( ! (size >= 2 && leftTree == Leaf && rightTree == Leaf ),
  		          "leftSize=%d (Leaf), rightSize=%d (Leaf)"
    		          .format(leftTuple._2, size - leftTuple._2 - 1) )
    		      Node(leftTree, root, rightTree)
    		    }
  		    )
        }
      }
    )
    
    getTreeOfSize = treesOfSize
    getTreeOfSizes = treesUpToSize
    
    val trees = treesOfSize
    
    object OMG {
	    var _res: light.Enum[Tree] = null
	    var elements: Seq[Tree] = null
	    def res = _res
	    def res_=(n: light.Enum[Tree]) = {
	      _res = n
	  		elements = (0 until res.size) map { res(_) }
	    }
	    def clue = (0 until res.size).map(res(_)).mkString(",")
    }
    import OMG._
    
    val profileRange = 10 to 10
    
    withLazyClue("Elements are: " + clue) {
      for (size <- 1 to 3) {
	      res = trees.getStream((size, Range(size, size - 1)))
	      res.size should be (0)
	      elements should be ('empty)
	      
	      res = trees.getStream((0, 1 to size))
	      res(0) should be (Leaf)
	      res.size should be (1)
      }

      res = trees.getStream(1, 1 to 3)
      res.size should be (3)
      elements should contain theSameElementsAs (1 to 3).map(
    		Node(Leaf, _, Leaf)
      )

      res = trees.getStream(2, 1 to 2)
      res.size should be (2)
      elements should contain allOf (
    		Node(Leaf, 1, Node(Leaf, 2, Leaf)),
    		Node(Node(Leaf, 1, Leaf), 2, Leaf)
      )

      res = trees.getStream(3, 1 to 3)
      res.size should be (5)
      elements should contain allOf (
    		Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
    		Node(Leaf, 1, Node(Node(Leaf, 2, Leaf), 3, Leaf))
      )

      res = trees.getStream(3, 1 to 4)
      res.size should be (5 * Binomial.binomialCoefficient(4, 3))
      elements should contain allOf (
    		Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
    		Node(Leaf, 1, Node(Node(Leaf, 2, Leaf), 3, Leaf))
      )

      for (size <- profileRange) {
	      profile("Getting stream for BST of size %d".format(size)) {
	      	res = trees.getStream(size, 1 to size)
	      }
	      profile("Claculating size for BST of size %d".format(size)) {
	      	res.size should be (Catalan.catalan(size))
	      }
	      profile("Getting elements for BST of size %d".format(size)) {
		      for (ind <- 0 until res.size) res(ind)
	      }
      }
      
    }
  }
  
}
