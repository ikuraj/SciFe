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
import org.scalatest.junit.JUnitRunner

import org.junit.{ Test, Ignore, BeforeClass, AfterClass }
import org.junit.Assert._
import org.junit.runner.RunWith

import insynth.common._
import insynth.testdomain.{ TestQueryBuilder => QueryBuilder, _ }
import insynth.util._
import insynth.util.format._
import insynth.util.logging._

import scala.language.postfixOps

//@RunWith(classOf[JUnitRunner])
class SortedListTest extends FunSuite with ShouldMatchers with ProfileLogger {
  
  import StreamableAST._
  import Structures._
  import Checks._
  
  val ones = Stream.continually(1)
  
  def sortedDifferent(size: Int) = 1
  
  def sortedRepeats(size: Int): Int =
    if (size > 1)
      Binomial.binomialCoefficient(size, 2) * 2 * sortedRepeats(size - 1) + sortedDifferent(size)
    else sortedDifferent(size)
    
  def sortedRepeats(n: Int, size: Int): Int =
    Binomial.binomialCoefficient(n, size) * sortedRepeats(size)

  def generateSortedLists(maxSize: Int, integers: Range) =
    generateLists(maxSize, integers) filter { l => l.sorted == l }
//  
////  println("sorted lists of size 8 = " + generateSortedLists(8, 1 to 8))
//    
  
  test("Enumeration of lists - extensive") {
    val intNode = Injecter(classOf[Int])
    val aggregatedIntNode = Generator(intNode)
//    val listNode = Combiner(classOf[List[Int]], aggregatedIntNode)

    val streamFactory = new OrderedStreamFactory[Any]

    val streamables = new StreamablesImpl(streamFactory)
    
    for (size <- 1 to 5) {
	    val intStream = (1 to size toStream) zip Stream.continually(1)
	    
	    val resultStream = streamables.getStreamListPairs(
	      aggregatedIntNode,
	      Map.empty,
	      Map.empty,
	      Map( classOf[Int] -> ( intStream, false ) )
	    )
	  
	      info("Streamable is: " + FormatStreamUtils(
	        streamables.getStreamableList(
		        aggregatedIntNode,
		        Map.empty,
		        Map.empty,
		        Map( classOf[Int] -> ( intStream, false ) )
	        )
	      ))
	       
	    // NOTE: + 1 for the empty list
	    val numberOfAllLists = 
	      (for (domainSize <- ( 1 to size )) yield {
	      	(1 /: (1 to domainSize)) { case (res, _) => res * size }
	      }).sum + 1
	      
	    val allLists =
	      for (maxSize <- 1 to size; list <- generateLists(maxSize, 1 to size)) yield list
	    
      profile( { for (res <- resultStream.take(numberOfAllLists)) res },
        "Sorted lists of size" + size)
	      
	    val resStream = resultStream.take(numberOfAllLists)
	    resStream.size should be (numberOfAllLists)
	
	    withClue ("Resulting stream is " + resStream.mkString(", ")) {
		    for (list <- allLists)
		      resStream.map( p => p._1.asInstanceOf[List[Int]] ) should contain (list)
      }
	        
	    nonDecreasing(resStream) should be (true)
	    noRepeat(resStream) should be (true)
    }
    
  }
  
  ignore("Number of sorted lists") {
  test("Number of sorted lists") {
    
    for (maxSize <- 1 to 4) {

      val sortedLists =         
        (for (size <- 1 to maxSize)
          yield generateSortedLists(size, 1 to maxSize)).flatten
      val calculatedNumber =
        (for (size <- 1 to maxSize)
          yield sortedRepeats(maxSize, size)).sum
  
      println("Number of sorted lists for size " + maxSize + " is: " + sortedLists.size)
      println(sortedLists.mkString(","))
      expectResult( calculatedNumber ) { sortedLists.size }
      
    }

  }
  }
  
  test("Enumeration of lists") {
    val intNode = Injecter(classOf[Int])
    val aggregatedIntNode = Generator(intNode)
//    val listNode = Combiner(classOf[List[Int]], aggregatedIntNode)

    val streamFactory = new OrderedStreamFactory[Any]

    val streamables = new StreamablesImpl(streamFactory)
    
    val intStream = Stream( 1, 2, 3 ) zip ones
    
    val resultStream = streamables.getStreamListPairs(
      aggregatedIntNode,
      Map.empty,
      Map.empty,
      Map( classOf[Int] -> ( intStream, false ) )
    )
       
    val resStream = resultStream.take(2000)
    
    for(ex <- List(
      (List(1, 2, 3), 4.0f),
      (List(1, 1, 2, 2, 1), 6.0f),
      (List(1, 1, 1, 1, 1, 1), 7.0f)
    ))
      assert(resStream.toSet contains ex, resultStream.take(100).mkString(", ") +
        " does not contain " + ex)
        
    nonDecreasing(resStream) should be (true)
    noRepeat(resStream) should be (true)
  }

  test("Enumeration of sorted lists") {   
    import CusList._
    
    val intValNode = Injecter(classOf[Int])
    val listValNode = Injecter(classOf[CusList])
    val justIntNode = Single(classOf[JustInt], intValNode)
    val listNode = Alternater(classOf[CusList], List(listValNode))
    val listParamNode = Aggregator(Seq(justIntNode, listNode))
    val consNode = Combiner(classOf[Cons], listParamNode)
    val filterListValNode = Filter(classOf[Cons], consNode)
    listNode.addStreamEl(filterListValNode)

    val streamFactory = new OrderedStreamFactory[Any]

    val streamables = new StreamablesImpl(streamFactory)

    val intStream = Stream( 1, 2, 3 ) zip ones
    val nilStream = Stream( (CusNil, 1) )

    val resultStream = streamables.getStreamPairs(
      filterListValNode,
      Map( justIntNode ->  { (a: Any) => a match {
        case e: Int => JustInt(e)
      }}),
      {
        case (clazz, (a: JustInt) :: (b: CusList) :: Nil) if clazz == classOf[Cons] =>
          Cons(a, b)
      },
      Map( classOf[Int] -> ( intStream, false ), classOf[CusList] -> ( nilStream, false ) ),
      Map(),
      Map( filterListValNode -> ( (e: Any) => 
        e match {
          case c: Cons => isSorted(c)
          case _ => false
        }
      ) )
    )
       
    val resStream = resultStream.take(1000)

    for(ex <- List(
      List(1, 2, 3),
      List(1, 1, 2, 2, 3),
      List(1, 1, 1, 1, 1, 1, 2)
    ): List[CusList])
      assert((resStream.map(_._1).toSet contains ex), resultStream.take(100).mkString(", ") +
        " does not contain " + ex)
    
    for(ex <- List(
      List(1, 3, 2),
      List(3, 2),
      List(3, 1),
      List(1, 1, 2, 2, 1),
      List(1, 1, 1, 1, 1, 2, 1)
    ): List[CusList])
      assert(! (resStream.map(_._1).toSet contains ex), resultStream.take(100).mkString(", ") +
        " does contain " + ex)
        
    nonDecreasing(resStream) should be (true)
    noRepeat(resStream) should be (true)
  }
  
  test("Number of enumerated custom lists") {
    val intValNode = Injecter(classOf[Int])
    val listValNode = Injecter(classOf[CusList])
    val justIntNode = Single(classOf[JustInt], intValNode)
    val listNode = Alternater(classOf[CusList], List(listValNode))
    val listParamNode = Aggregator(Seq(justIntNode, listNode))
    val consNode = Combiner(classOf[Cons], listParamNode)
    listNode.addStreamEl(consNode)
  
    val streamFactory = new OrderedStreamFactory[Any]
  
    val streamables = new StreamablesImpl(streamFactory)
    
    val nilStream = Stream( (CusNil, 1) )

    import CusList._
//    Attribution.initTree(filterListValNode)

    for (intRange <- 1 to 3) {
      val intStream = 1 to intRange toStream
      val resultStream = streamables.getStreamPairs(
        consNode,
        Map( justIntNode ->  { (a: Any) => a match {
          case e: Int => JustInt(e)
        }}),
        {
          case (clazz, (a: JustInt) :: (b: CusList) :: Nil) if clazz == classOf[Cons] =>
            Cons(a, b)
        },
        Map( classOf[Int] -> ( intStream zip Stream.continually(1), false ), classOf[CusList] -> ( nilStream, false ) ),
        Map()
      )
         
      val resStream = resultStream.take(1000).map(_._1)
        
      for (size <- 1 to 5) {
        val generatedLists = resStream.filter(l => CusList.size(l.asInstanceOf[CusList]) == size)
        val domainSize = (1 /: (1 to size)) { case (res, _) => res * intRange }
        
        
        withClue ("Enumerated\n" + generatedLists.mkString("\n")) {
          generatedLists.size should be (domainSize) 
        }
      }
    }
  }
  
  test("Number of enumerated custom lists, with filter node") {

    val intValNode = Injecter(classOf[Int])
    val listValNode = Injecter(classOf[CusList])
    val justIntNode = Single(classOf[JustInt], intValNode)
    val listNode = Alternater(classOf[CusList], List(listValNode))
    val listParamNode = Aggregator(Seq(justIntNode, listNode))
    val consNode = Combiner(classOf[Cons], listParamNode)
    val filterListValNode = Filter(classOf[Cons], consNode)
    listNode.addStreamEl(filterListValNode)
  
    val streamFactory = new OrderedStreamFactory[Any]
  
    val streamables = new StreamablesImpl(streamFactory)
    
    val nilStream = Stream( CusNil ) zip ones

    import CusList._
//    Attribution.initTree(filterListValNode)

    for (intRange <- 1 to 5) {
      val intStream = 1 to intRange toStream
      val resultStream = streamables.getStreamPairs(
        filterListValNode,
        Map( justIntNode ->  { (a: Any) => a match {
          case e: Int => JustInt(e)
        }}),
        {
          case (clazz, (a: JustInt) :: (b: CusList) :: Nil) if clazz == classOf[Cons] =>
            Cons(a, b)
        },
        Map( classOf[Int] -> ( intStream zip ones, false ), classOf[CusList] -> ( nilStream, false ) ),
        Map(),
        Map( filterListValNode -> ( (e: Any) => true ) )
      )
         
      val resStream = resultStream.take(4000).map(_._1)
        
      for (size <- 1 to 5) {
        val generatedLists = resStream.filter(l => CusList.size(l.asInstanceOf[CusList]) == size)
        val domainSize = (1 /: (1 to size)) { case (res, _) => res * intRange }
        
        withClue ("Enumerated\n" + generatedLists.mkString("\n")) {
          generatedLists.size should be (domainSize) 
        }
      }
    }
  }

  test("Number of enumerated sorted lists") {

    val intValNode = Injecter(classOf[Int])
    val listValNode = Injecter(classOf[CusList])
    val justIntNode = Single(classOf[JustInt], intValNode)
    val listNode = Alternater(classOf[CusList], List(listValNode))
    val listParamNode = Aggregator(Seq(justIntNode, listNode))
    val consNode = Combiner(classOf[Cons], listParamNode)
    val filterListValNode = Filter(classOf[Cons], consNode)
    listNode.addStreamEl(filterListValNode)
  
    val streamFactory = new OrderedStreamFactory[Any]
  
    val streamables = new StreamablesImpl(streamFactory)
    
    val nilStream = Stream( CusNil ) zip ones
    import CusList._
//    Attribution.initTree(filterListValNode)

    for (intRange <- 1 to 5) {
      val intStream = 1 to intRange toStream
      val resultStream = streamables.getStreamPairs(
        filterListValNode,
        Map( justIntNode ->  { (a: Any) => a match {
          case e: Int => JustInt(e)
        }}),
        {
          case (clazz, (a: JustInt) :: (b: CusList) :: Nil) if clazz == classOf[Cons] =>
            Cons(a, b)
        },
        Map( classOf[Int] -> ( intStream zip ones, false ), classOf[CusList] -> ( nilStream, false ) ),
        Map(),
        Map( filterListValNode -> ( (e: Any) => 
          e match {
            case c: Cons => isSorted(c)
            case _ => false
          }
        ) )
      )
         
      val resStream = resultStream.take(1000).map(_._1)
        
      for (size <- 1 to 5) {
        val sortedLists = generateSortedLists(size, 1 to intStream.size)
        val generatedLists = resStream.filter(l => CusList.size(l.asInstanceOf[CusList]) == size)
        
        withClue ("Generated naively:\n" + sortedLists.mkString("\n") +
          "\nEnumerated\n" + generatedLists.mkString("\n")) {
          generatedLists.size should be (sortedLists.size) 
        }
      }
    }
  }

}