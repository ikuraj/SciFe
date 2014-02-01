package insynth.streams.ordered
package memory

import scala.collection.mutable
import scala.util.Random

import insynth.structures._
import insynth.reconstruction.stream._
import insynth.reconstruction._
import insynth.attrgrammar._

import insynth.util._
import insynth.util.logging._
import insynth.util.format._

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitSuite
import org.scalatest.matchers._
import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import scala.language.implicitConversions
import scala.language.postfixOps

@RunWith(classOf[JUnitRunner])
class StreamCombinationsMemoryTest extends FunSuite with ShouldMatchers with HasLogger {

  val rnd = new Random(System.currentTimeMillis())

  implicit def listToIndexedSeq[T](list: List[T]) = list.toIndexedSeq

  import Utils._
  import Checks._
  
//  @Test
//  def testBinaryStreamLoop {
//    val stream1 = getSingleStream(2)
//
//    val rr = LazyRoundRobbin(List(stream1))
//
//    val stream2 = getSingleStream(Stream(1), false)
//
//    val bs = BinaryStream(rr, stream2) {
//      (x, y) => x + y
//    }
//
//    rr addStreamable bs
//
//    rr.initialize
//
//    // 2 will stay since the right stream in binary stream will take 1 as a value and dismiss its iterator
//    assertCollectedWithExceptions(rr, 100, last = 1, testFun = { (i: Int) => ! (Set(2) contains i) } )
//    getCollectedCount(rr, 100) should be >= 97 
//  }
//
//  @Test
//  def testBinaryStreamLoopMod {
//    val stream1 = getSingleStream(2)
//
//    val rr = LazyRoundRobbin(List(stream1))
//
//    val stream2 = getSingleStream(Stream(3, 4), false)
//
//    val bs = BinaryStream(rr, stream2) {
//      (x, y) => x + y
//    }
//
//    rr addStreamable bs
//
//    rr.initialize
//
//    // 2,  and two elements on two left iterators
//    getCollectedCount(rr, 100) should be >= 97
//    assertCollectedWithExceptions(rr, 100, testFun = { (i: Int) => ! (Set(2, 6, 9) contains i) } )
//  }
//
//  @Test
//  def testWithBinaryStream2 {
//    val streamable1 = getSingleStream(Stream(1, 3, 7), false)
//    val streamable2 = getSingleStream(Stream(2, 3, 6), false)
//
//    val streamable3 = LazyRoundRobbin(List(streamable1))
//    val streamable4 = LazyRoundRobbin(List(streamable2))
//
//    {
//      val bs = BinaryStream(streamable3, streamable4) {
//        (x, y) => x + y
//      }
//      
//      streamable3 addStreamable bs
//      streamable4 addStreamable bs
//      streamable3 addStreamable streamable4
//
//      streamable3.initialize
//      streamable4.initialize
//
//      getCollectedCount(bs, 100) should be >= 90
//      getCollectedCount(bs, 1000) should be >= 990
//    }
//  }
//
//  @Test
//  def testRecursionWithFilter {
//    val innerStream = Stream(0, 1, 2)
//    val stream1 = WrapperStream( innerStream zip Stream.continually(0) )
//    val rr = LazyRoundRobbin(List(stream1))
//    val us = UnaryStream(rr, { (x: Int) => x + 2 })
//    val filterOdds = FilterStream.counted(us, { (x: Int) => x % 2 == 0 })
//
//    rr addFilterable filterOdds
//
//    rr.initialize
//    
//    getCollectedCount(filterOdds, 100) should be >= 90
//  }

  test("FilterStream, unary streams, memoized, non filtered should be GCed") {
    val single0 = Singleton(new Integer(0))
    val lazyStream = LazyRoundRobbin(Seq( single0 ))
    
    var toBeCollected = mutable.MutableList[Integer]()
    val us = UnaryStream( lazyStream, { (v: Integer) =>
      toBeCollected += v; v } )
    val filteredStream =
      FilterStream.memoized(
        us, { (i: Integer) => ((i % 3) % 2) == 0 }
      )

    val finiteStream = WrapperStream( Seq(1, 2, 3) map (new Integer (_)) zipWithIndex )
    val binaryStream = BinaryStream(
      finiteStream, filteredStream
    ) ( (l, r) => new Integer(l + r) )
    lazyStream.addStreamable( binaryStream )
    fine("Streamable is: " + FormatStreamUtils(filteredStream))
    
    val stream = filteredStream.getValuedStream
    
    // force 10 enumerations
    stream.take(10).size
    
    withClue ( stream.take(10).mkString(", ") ) {
	    (toBeCollected filterNot filteredStream.filterFun).size should be > 0
	    val tester = MemoryLeak( toBeCollected filterNot filteredStream.filterFun )
	    toBeCollected = null
	
	    tester.countCollected should be >= 10
    }
  }

//  test("rbtrees") {
//    import insynth.util.Structures._
//    import StreamableAST._
//    import RedBlackTrees._
//	  
//	  type Weight = Int
//    
//    val leafNode = Injecter(classOf[Tree])
//    val booleanNode = Injecter(classOf[Boolean])
//    val intNode = Injecter(classOf[Int])
//    val chooserNode = Alternater(classOf[Tree], List(leafNode))
//    val filteredTrees = Filter(classOf[Cons], chooserNode)
//    val treeParamNode = Aggregator(Seq(filteredTrees, intNode, filteredTrees, booleanNode))
//    
//    val consNode =
//      Single( classOf[Node], Combiner(classOf[Node], treeParamNode) )
//    chooserNode.addStreamEl(consNode)
//
//    implicit def anyToRBTree(a: Any) = a.asInstanceOf[Tree]
//    
//    // NOTE making this 1 to 5 throws StackOverflowError
//    for(currentSize <- 6 to 6) {
//      val (treeSize, correctNumber) = (currentSize, numberOfTress(currentSize))
//      info("For currentSize %d, number of trees should be %d".format(treeSize, correctNumber))
//  
//      val nilStream = Stream( (Leaf, 1) )
//      val intStream = (1 to treeSize) zip Stream.continually(1) toStream
//      val booleanStream = Stream(true, false) zip Stream.continually(1)
//      
//      val streamFactory = new OrderedStreamFactory[Any]
//      val streamables = new StreamablesImpl(streamFactory)
//    
//      var allTrees = mutable.MutableList[Node]()
//    
//	    val streamable = streamables.getStreamable(
//	      filteredTrees,
//	      Map(consNode -> { tree: Any => allTrees += tree.asInstanceOf[Node]; tree } ),
//	      constructRBTree,
//	      Map( classOf[Tree] -> ( nilStream, false ), classOf[Int] -> ( intStream, false ), 
//	          classOf[Boolean] -> ( booleanStream, false )),
//	      Map(),
//	      Map( filteredTrees -> ( (e: Any) => invariant(e.asInstanceOf[Tree]) ) )
//	    )
//      fine("Streamable is: " + FormatStreamUtils(streamable))
//      val resultStream = streamables.extractPairStream(streamable)
//    
//	    var resList: List[(Any, Weight)] = null
//	    def checkSize = (p: (Any, Weight)) => RedBlackTrees.size(p._1.asInstanceOf[Tree]) == treeSize
//      
//	    var foundAll = false
//	    var bound = -1
//	    for (toTake <- 1 to 10000; if !foundAll) {
//		    bound = toTake
//		    val startTime = System.currentTimeMillis
//		    resList = resultStream.take(toTake).toList
//		    val duration = System.currentTimeMillis - startTime
//
//		    foundAll = resList.count(checkSize) == correctNumber
//		    fine("Count of tree of size " + treeSize + " is: " +
//	        resList.count(checkSize))
//	    }
//      info("Count of tree of size " + treeSize + " is found: " + resList.count(checkSize))
//	        
//	    resList = resultStream.take(bound).toList
//	    val tester = {
//        val objects = for (el <- allTrees; if ! invariant(el.asInstanceOf[Tree])) yield el
//        MemoryLeak(objects)
//      }
//	    
//      val allTreesNum = allTrees.size
//      info("All trees : " + allTreesNum)
//      
//      allTrees = null
//      resList = null
////      println("done")
////      Console.readLine
//
//      val collected = tester.countCollected
//	    collected should be >= (allTreesNum - 40)
//    }
//    
//  }
  
}