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
  
  test("Enumeration of tree (just shape)") {   
    import TreeShapes._
    
    val leafNode = Injecter(classOf[Tree])
    val chooserNode = Alternater(classOf[Tree], List(leafNode))
    val treeParamNode = Aggregator(Seq(chooserNode, chooserNode))
    val consNode = Combiner(classOf[Node], treeParamNode)
    chooserNode.addStreamEl(consNode)

    val streamFactory = new OrderedStreamFactory[Any]

    val streamables = new StreamablesIml(streamFactory)

    val nilStream = Stream( (Leaf, 1) )

    val resultStream = streamables.getStreamPairs(
      chooserNode,
      Map(),
      {
        case (clazz, (a: Tree) :: (b: Tree) :: Nil) if clazz == classOf[Node] =>
          Node(a, b)
      },
      Map( classOf[Tree] -> ( nilStream, false ) ),
      Map()
    )
       
    val resStream = resultStream.take(1000)

    val n1 = Node(Leaf, Leaf)
    val n2 = Node(Leaf, n1)
    val n3 = Node(n2, n1)
    
    for(ex <- List(
        Leaf, n1, n2, n3
    ))
      assert((resStream.map(_._1).toSet contains ex), resultStream.take(100).mkString(", ") +
        " does not contain " + ex)
    
    nonDecreasing(resStream) should be (true)
    noRepeat(resStream) should be (true)
  }
  
  test("Enumeration of tree, weights (just shape)") {   
    import TreeShapes._
    
    val leafNode = Injecter(classOf[Tree])
    val chooserNode = Alternater(classOf[Tree], List(leafNode))
    val treeParamNode = Aggregator(Seq(chooserNode, chooserNode))
    val consNode = Combiner(classOf[Node], treeParamNode)
    chooserNode.addStreamEl(consNode)

    val streamFactory = new OrderedStreamFactory[Any]

    val streamables = new StreamablesIml(streamFactory)

    val nilStream = Stream( (Leaf, 1) )

    val resultStream = streamables.getStreamPairs(
      chooserNode,
      Map(),
      {
        case (clazz, (a: Tree) :: (b: Tree) :: Nil) if clazz == classOf[Node] =>
          Node(a, b)
      },
      Map( classOf[Tree] -> ( nilStream, false ) ),
      Map()
    )
       
    val resStream = resultStream.take(1000)
    
    for ((e, w) <- resStream) 
      withClue("Tree " + e + " has size " + TreeShapes.size(e.asInstanceOf[Tree]) + " not " + w) {
        TreeShapes.size(e.asInstanceOf[Tree]) == w should be (true)
      }
  }
  
  test("Enumeration of tree, number (just shape)") {   
    import TreeShapes._
    
    val leafNode = Injecter(classOf[Tree])
    val chooserNode = Alternater(classOf[Tree], List(leafNode))
    val treeParamNode = Aggregator(Seq(chooserNode, chooserNode))
    val consNode = Combiner(classOf[Node], treeParamNode)
    chooserNode.addStreamEl(consNode)

    val streamFactory = new OrderedStreamFactory[Any]

    val streamables = new StreamablesIml(streamFactory)

    val nilStream = Stream( (Leaf, 1) )

    val resultStream = streamables.getStreamPairs(
      chooserNode,
      Map(),
      {
        case (clazz, (a: Tree) :: (b: Tree) :: Nil) if clazz == classOf[Node] =>
          Node(a, b)
      },
      Map( classOf[Tree] -> ( nilStream, false ) ),
      Map()
    )
       
    val resStream = resultStream.take(2 * 58786)
    
    for ((num, size) <- List(1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862,
      16796, 58786/*, 208012, 742900, 2674440*/).zipWithIndex.tail) {
      withClue(resStream take 50 mkString(",")) {
        (resStream filter { p => sizeJustNodes(p._1.asInstanceOf[Tree]) == size } size) should be (num)
      }
    }
  }

}