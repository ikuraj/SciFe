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
import org.scalatest.prop.Checkers
import org.scalacheck._

import org.junit.{ Test, Ignore, BeforeClass, AfterClass }
import org.junit.Assert._

import insynth.common._
import insynth.testdomain.{ TestQueryBuilder => QueryBuilder, _ }
import insynth.util._

import scala.language.postfixOps

class RedBlackTreeTest extends FunSuite with ShouldMatchers {
  
  import Structures._
  import StreamableAST._
  import Checks._
  
  test("Enumeration of RB trees (without enforcing invariant)") {   
    import RedBlackTrees._
    
    val leafNode = Injecter(classOf[Tree])
    val booleanNode = Injecter(classOf[Boolean])
    val intNode = Injecter(classOf[Int])
    val chooserNode = Alternater(classOf[Tree], List(leafNode))
    val treeParamNode = Aggregator(Seq(chooserNode, intNode, chooserNode, booleanNode))
    val consNode = Combiner(classOf[Node], treeParamNode)
    chooserNode.addStreamEl(consNode)

    val streamFactory = new OrderedStreamFactory[Any]

    val streamables = new StreamablesImpl(streamFactory)

    val nilStream = Stream( (Leaf, 1) )
    val intStream = fromOne zip fromOne
    val booleanStream = Stream(true, false) zip fromOne
    
    val resultStream = streamables.getStreamPairs(
      chooserNode,
      Map(),
      {
        case (clazz, (a: Tree) :: (v: Int) :: (b: Tree) :: (c: Boolean) :: Nil)
          if clazz == classOf[Node] =>
          Node(a, v, b, c)
      },
      Map( classOf[Tree] -> ( nilStream, false ), classOf[Int] -> ( intStream, true ), 
          classOf[Boolean] -> ( booleanStream, false )),
      Map()
    )
       
    val resStream = resultStream.take(100000)

    val n1 = Node(Leaf, 1, Leaf, true)
    val n2 = Node(Leaf, 2, n1, false)
    val n3 = Node(n1, 3, n2, true)
    val n4 = Node(n3, 1, n1, false)
    
    val treeList = List( Leaf, n1, n2, n3 )
    
    for(ex <- treeList)
      assert((resStream.map(_._1).toSet contains ex), resultStream.take(100).mkString(", ") +
        " does not contain " + ex)
    
    nonDecreasing(resStream) should be (true)
    noRepeat(resStream) should be (true)
  }
  
  test("Enumeration of RB trees (with enforcing invariant)") { 
    import Gen._  
    import RedBlackTrees._
    
    val leafNode = Injecter(classOf[Tree])
    val booleanNode = Injecter(classOf[Boolean])
    val intNode = Injecter(classOf[Int])
    val chooserNode = Alternater(classOf[Tree], List(leafNode))
    val treeParamNode = Aggregator(Seq(chooserNode, intNode, chooserNode, booleanNode))
    val consNode = Combiner(classOf[Node], treeParamNode)
    val filteredTrees = Filter(classOf[Cons], chooserNode)
    chooserNode.addStreamEl(consNode)

    val streamFactory = new OrderedStreamFactory[Any]

    val streamables = new StreamablesImpl(streamFactory)

    val nilStream = Stream( (Leaf, 1) )
    val intStream = (1 to 5) zip (1 to 5) toStream
    val booleanStream = Stream(true, false) zip fromOne
    
    val resultStream = streamables.getStreamPairs(
      filteredTrees,
      Map(),
      {
        case (clazz, (a: Tree) :: (v: Int) :: (b: Tree) :: (c: Boolean) :: Nil)
          if clazz == classOf[Node] =>
          Node(a, v, b, c)
      },
      Map( classOf[Tree] -> ( nilStream, false ), classOf[Int] -> ( intStream, true ), 
          classOf[Boolean] -> ( booleanStream, false )),
      Map(),
      Map( filteredTrees -> ( (e: Any) => invariant(e.asInstanceOf[Tree]) ) )
    )
       
    val resStream = resultStream.take(1000)
    nonDecreasing(resStream) should be (true)
    noRepeat(resStream) should be (true)
    assert ( resStream.map(_._1.asInstanceOf[Tree]) forall invariant )

    val rbTreeGen =
      for {
        // trees up to size 5
        size <- Gen.choose(1, 5)
        values <- Gen.listOfN(size, Gen.choose(1, 3))
      } yield {
        val rbMap = RBMap(values map (x => (x, null)): _*)
        
        rbMap2rbTree(rbMap)
      }
      
    Prop.forAll(rbTreeGen) ( rbTree =>        
      invariant(rbTree) && (resStream.map(_._1).toSet contains rbTree) 
    ) check
    
  }

  test("Enumeration of RB trees (count)") { 
    import Gen._  
    import RedBlackTrees._
    
    val leafNode = Injecter(classOf[Tree])
    val booleanNode = Injecter(classOf[Boolean])
    val intNode = Injecter(classOf[Int])
    val chooserNode = Alternater(classOf[Tree], List(leafNode))
    val treeParamNode = Aggregator(Seq(chooserNode, intNode, chooserNode, booleanNode))
    val consNode = Combiner(classOf[Node], treeParamNode)
    val filteredTrees = Filter(classOf[Cons], chooserNode)
    chooserNode.addStreamEl(consNode)

    val streamFactory = new OrderedStreamFactory[Any]

    val streamables = new StreamablesImpl(streamFactory)

    val nilStream = Stream( (Leaf, 1) )
    val intStream = (1 to 5) zip (1 to 5) toStream
    val booleanStream = Stream(true, false) zip fromOne
    
    val resultStream = streamables.getStreamPairs(
      filteredTrees,
      Map(),
      {
        case (clazz, (a: Tree) :: (v: Int) :: (b: Tree) :: (c: Boolean) :: Nil)
          if clazz == classOf[Node] =>
          Node(a, v, b, c)
      },
      Map( classOf[Tree] -> ( nilStream, false ), classOf[Int] -> ( intStream, true ), 
          classOf[Boolean] -> ( booleanStream, false )),
      Map(),
      Map( filteredTrees -> ( (e: Any) => invariant(e.asInstanceOf[Tree]) ) )
    )
       
    val startTime = System.currentTimeMillis
    val resList = resultStream.take(85).toList
    val duration = System.currentTimeMillis - startTime
    
    println("Trees in " + duration + " :" + resList.mkString("\n"))
    
  }

  test("Enumeration of RB trees (timed)") { 
    import Gen._  
    import RedBlackTrees._
    
    val leafNode = Injecter(classOf[Tree])
    val booleanNode = Injecter(classOf[Boolean])
    val intNode = Injecter(classOf[Int])
    val chooserNode = Alternater(classOf[Tree], List(leafNode))
    val treeParamNode = Aggregator(Seq(chooserNode, intNode, chooserNode, booleanNode))
    val consNode = Combiner(classOf[Node], treeParamNode)
    val filteredTrees = Filter(classOf[Cons], chooserNode)
    chooserNode.addStreamEl(consNode)

    val streamFactory = new OrderedStreamFactory[Any]

    val streamables = new StreamablesImpl(streamFactory)

    val nilStream = Stream( (Leaf, 1) )
    val intStream = (1 to 5) zip (1 to 5) toStream
    val booleanStream = Stream(true, false) zip fromOne
    
    val resultStream = streamables.getStreamPairs(
      filteredTrees,
      Map(),
      {
        case (clazz, (a: Tree) :: (v: Int) :: (b: Tree) :: (c: Boolean) :: Nil)
          if clazz == classOf[Node] =>
          Node(a, v, b, c)
      },
      Map( classOf[Tree] -> ( nilStream, false ), classOf[Int] -> ( intStream, true ), 
          classOf[Boolean] -> ( booleanStream, false )),
      Map(),
      Map( filteredTrees -> ( (e: Any) => invariant(e.asInstanceOf[Tree]) ) )
    )
       
    val startTime = System.currentTimeMillis
    val resList = resultStream.take(85).toList
    val duration = System.currentTimeMillis - startTime
    
    println("Trees in " + duration + " :" + resList.mkString("\n"))
    
  }
  
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

}
