//package insynth.enumeration
//package reverse
//
//import org.scalatest._
//import org.scalatest.prop._
//import org.scalatest.matchers._
//
//import insynth.{ enumeration => e }
//import memoization._
//import e.dependent._
//
//import util._
//import insynth.util.logging._
//import insynth.util._
//  
//import scala.language.existentials
//
//class BinarySearchTreeTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
//	HasLogger with ProfileLogger {  
//  import Checks._
//  import Structures._
//  import BSTrees._
//  
//  def bstTests(trees: Depend[(Int, Range), Tree]) {
//    object OMG {
//      var _res: Enum[Tree] = null
//      var elements: Seq[Tree] = null
//      def res = _res
//      def res_=(n: Enum[Tree]) = {
//        _res = n
//        elements = (0 until res.size) map { res(_) }
//      }
//      def clue = (0 until res.size).map(res(_)).mkString(",")
//    }
//    import OMG._
//    
//    val profileRange = 4 to 9
//    
//    withLazyClue("Elements are: " + clue) {
////      for (size <- 1 to 3) {
////        res = trees.getStream((size, Range(size, size - 1)))
////        res.size should be (0)
////        elements should be ('empty)
////        
////        res = trees.getStream((0, 1 to size))
////        res(0) should be (Leaf)
////        res.size should be (1)
////      }
////
////      res = trees.getStream(1, 1 to 3)
////      res.size should be (3)
////      elements should contain theSameElementsAs (1 to 3).map(
////        Node(Leaf, _, Leaf)
////      )
////
////      res = trees.getStream(2, 1 to 2)
////      res.size should be (2)
////      elements should contain allOf (
////        Node(Leaf, 1, Node(Leaf, 2, Leaf)),
////        Node(Node(Leaf, 1, Leaf), 2, Leaf)
////      )
////
////      res = trees.getStream(3, 1 to 3)
////      res.size should be (5)
////      elements should contain allOf (
////        Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
////        Node(Leaf, 1, Node(Node(Leaf, 2, Leaf), 3, Leaf))
////      )
////
////      res = trees.getStream(3, 1 to 4)
////      res.size should be (5 * Binomial.binomialCoefficient(4, 3))
////      elements should contain allOf (
////        Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
////        Node(Leaf, 1, Node(Node(Leaf, 2, Leaf), 3, Leaf))
////      )
//
//      for (size <- profileRange) {
//        profile("Getting stream for BST of size %d".format(size)) {
//          res = trees.getEnum(size, 1 to size)
//        }
//        profile("Claculating size for BST of size %d".format(size)) {
//          res.size should be (Catalan.catalan(size))
//        }
//        profile("Getting elements for BST of size %d".format(size)) {
//          for (ind <- 0 until res.size) res(ind)
//        }
//        
//        assert( (for (ind <- 0 until res.size) yield res(ind)).forall( invariant(_) ) )
//      }
//      
//    }
//
//  }
//
//  private def constructEnumerator(implicit ms: MemoizationScope = null) = {
//    val rootProducer = dependent.Reverser(
//      (range: Range) => {
//        Reverser(range)
//      })
//
//    val sizeProducer = dependent.Reverser(
//      (size: Int) => {
//        Reverser(0 until size)
//      })
//
//    Depend.memoized(
//      (self: Depend[(Int, Range), Tree], pair: (Int, Range)) => {
//        val (size, range) = pair
//
//        if (size <= 0) Reverser(Leaf)
//        else if (size == 1) Reverser(range map { v => Node(Leaf, v, Leaf) })
//        else {
//          val roots = rootProducer.getEnum(range)
//          val leftSizes = sizeProducer.getEnum(size)
//
//          val rootLeftSizePairs = e.Product(leftSizes, roots)
//
//          val leftTrees = InMap(self, { (par: (Int, Int)) =>
//            val (leftSize, median) = par
//            (leftSize, range.start to (median - 1))
//          })
//
//          val rightTrees =
//            InMap(self, { (par: (Int, Int)) =>
//              val (leftSize, median) = par
//              (size - leftSize - 1, (median + 1) to range.end)
//            })
//
//          val leftRightPairs =
//            Product(leftTrees, rightTrees)
//
//          val allNodes =
//            new dependent.ChainFinite(rootLeftSizePairs, leftRightPairs)
//        		
//        		
////              (p1: (Int, Int), p2: (Tree, Tree)) => {
////                val ((leftSize, currRoot), (leftTree, rightTree)) = (p1, p2)
////
////                Node(leftTree, currRoot, rightTree)
////              })
//
//          allNodes
//        }
//      })
//  }
//  
//  test("enumerator regular enumeration") {
//    bstTests( constructEnumerator )
//  }
//  
//  test("enumerator reverse enumeration") {
//    bstTests( constructEnumerator )
//  }
//
//}
