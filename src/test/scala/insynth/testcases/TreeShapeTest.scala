package insynth
package testcases

import scala.collection.mutable.{ Map => MutableMap, Set => MutableSet }
import org.kiama.attribution.Attribution

import insynth.structures._
import insynth.reconstruction.stream._
import insynth.reconstruction._
import insynth.attrgrammar._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import org.junit.{ Test, Ignore, BeforeClass, AfterClass }
import org.junit.Assert._

import insynth.common._
import insynth.testdomain.{ TestQueryBuilder => QueryBuilder, _ }
import insynth.util._

import scala.language.postfixOps

class TreeShapesTest extends FunSuite with ShouldMatchers {
  
  import Structures._
  import StreamableAST._
  import Checks._
  
//  test("Enumeration of tree (just shape)") {   
//    import TreeShapes._
//    
//    val leafNode = Injecter(classOf[Tree])
//    val chooserNode = Alternater(classOf[Tree], List(leafNode))
//    val treeParamNode = Aggregator(Seq(chooserNode, chooserNode))
//    val consNode = Combiner(classOf[Node], treeParamNode)
//    chooserNode.addStreamEl(consNode)
//
//    val streamFactory = new OrderedStreamFactory[Any]
//
//    val streamables = new StreamablesImpl(streamFactory)
//
//    val nilStream = Stream( (Leaf, 1) )
//
//    val resultStream = streamables.getStreamPairs(
//      chooserNode,
//      Map(),
//      {
//        case (clazz, (a: Tree) :: (b: Tree) :: Nil) if clazz == classOf[Node] =>
//          Node(a, b)
//      },
//      Map( classOf[Tree] -> ( nilStream, false ) ),
//      Map()
//    )
//       
//    val resStream = resultStream.take(1000)
//
//    val n1 = Node(Leaf, Leaf)
//    val n2 = Node(Leaf, n1)
//    val n3 = Node(n2, n1)
//    
//    for(ex <- List(
//        Leaf, n1, n2, n3
//    ))
//      assert((resStream.map(_._1).toSet contains ex), resultStream.take(100).mkString(", ") +
//        " does not contain " + ex)
//    
//    nonDecreasing(resStream) should be (true)
//    noRepeat(resStream) should be (true)
//  }
//  
//  test("Enumeration of tree, weights (just shape)") {   
//    import TreeShapes._
//    
//    val leafNode = Injecter(classOf[Tree])
//    val chooserNode = Alternater(classOf[Tree], List(leafNode))
//    val treeParamNode = Aggregator(Seq(chooserNode, chooserNode))
//    val consNode = Combiner(classOf[Node], treeParamNode)
//    chooserNode.addStreamEl(consNode)
//
//    val streamFactory = new OrderedStreamFactory[Any]
//
//    val streamables = new StreamablesImpl(streamFactory)
//
//    val nilStream = Stream( (Leaf, 1) )
//
//    val resultStream = streamables.getStreamPairs(
//      chooserNode,
//      Map(),
//      {
//        case (clazz, (a: Tree) :: (b: Tree) :: Nil) if clazz == classOf[Node] =>
//          Node(a, b)
//      },
//      Map( classOf[Tree] -> ( nilStream, false ) ),
//      Map()
//    )
//       
//    val resStream = resultStream.take(1000)
//    
//    for ((e, w) <- resStream) 
//      withClue("Tree " + e + " has size " + TreeShapes.size(e.asInstanceOf[Tree]) + " not " + w) {
//        TreeShapes.size(e.asInstanceOf[Tree]) == w should be (true)
//      }
//  }
//  
//  test("Enumeration of tree, number (just shape)") {   
//    import TreeShapes._
//    
//    val leafNode = Injecter(classOf[Tree])
//    val chooserNode = Alternater(classOf[Tree], List(leafNode))
//    val treeParamNode = Aggregator(Seq(chooserNode, chooserNode))
//    val consNode = Combiner(classOf[Node], treeParamNode)
//    chooserNode.addStreamEl(consNode)
//
//    val streamFactory = new OrderedStreamFactory[Any]
//
//    val streamables = new StreamablesImpl(streamFactory)
//
//    val nilStream = Stream( (Leaf, 1) )
//
//    val resultStream = streamables.getStreamPairs(
//      chooserNode,
//      Map(),
//      {
//        case (clazz, (a: Tree) :: (b: Tree) :: Nil) if clazz == classOf[Node] =>
//          Node(a, b)
//      },
//      Map( classOf[Tree] -> ( nilStream, false ) ),
//      Map()
//    )
//       
//    val resStream = resultStream.take(2 * 58786)
//    
//    for ((num, size) <- List(1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862,
//      16796, 58786/*, 208012, 742900, 2674440*/).zipWithIndex.tail) {
//      withClue(resStream take 50 mkString(",")) {
//        (resStream filter { p => sizeJustNodes(p._1.asInstanceOf[Tree]) == size } size) should be (num)
//      }
//    }
//  }
  
  test("Enumeration of tree, BST invariant") {   
    import BSTrees._
    
    val intNode = Injecter(classOf[Int])
    val leafNode = Injecter(classOf[Tree])
    val chooserNode = Alternater(classOf[Tree], List(leafNode))
    val filteredTrees = Filter(classOf[Cons], chooserNode)
    val treeParamNode = Aggregator(Seq(filteredTrees, intNode, filteredTrees))
    val consNode = Combiner(classOf[Node], treeParamNode)
    chooserNode.addStreamEl(consNode)
    
    val mainNode = filteredTrees

    val streamFactory = new OrderedStreamFactory[Any]
    val streamables = new StreamablesImpl(streamFactory)

    val MAX_SIZE = 5
    val correctNumber = 42
    
    val intStream = (1 to MAX_SIZE) zip Stream.continually(1) toStream
    val nilStream = Stream( (Leaf, 1) )

    val resultStream = streamables.getStreamPairs(
      mainNode,
      Map(),
      {
        case (clazz, (a: Tree) :: (v: Int) :: (b: Tree) :: Nil) if clazz == classOf[Node] =>
          Node(a, v, b)
      },
      Map( classOf[Tree] -> ( nilStream, false ), classOf[Int] -> ( intStream, false ) ),
      Map(),
      Map( filteredTrees -> ( (e: Any) => invariant(e.asInstanceOf[Tree]) ) )
    )
       
    type Weight = Int
    var resList: List[(Any, Weight)] = null
    def checkSize = (p: (Any, Weight)) => BSTrees.size(p._1.asInstanceOf[Tree]) == MAX_SIZE
    
    var foundAll = false
    var bound = -1
    for (toTake <- 1 to 10000; if !foundAll) {
      bound = toTake
      val startTime = System.currentTimeMillis
      resList = resultStream.take(toTake).toList
      val duration = System.currentTimeMillis - startTime

      foundAll = resList.count(checkSize) == correctNumber
    }
        
    val startTime = System.currentTimeMillis
    resList = resultStream.take(bound).toList
    val duration = System.currentTimeMillis - startTime
    println("Got %d trees in %s".format(bound, duration.toString))
    
    assert( resList.forall(p => BSTrees.invariant(p._1.asInstanceOf[Tree])) )
    resList.size should be (resList.map(_._1).distinct.size)
    
    resList.count(checkSize) should be (correctNumber)
  }

}