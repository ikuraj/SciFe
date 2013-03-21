package insynth.reconstruction.stream

import insynth.reconstruction.{ stream => lambda }

import org.scalatest.junit.JUnitSuite
import org.junit.{ Test, Ignore }
import org.junit.Assert._

import insynth.util.format._

import insynth.common.{ CommonProofTrees, CommonDeclarations, CommonLambda }

class TransformerTest extends JUnitSuite {

  import CommonDeclarations._
  import CommonProofTrees._
  import CommonLambda._
    
  val transformer = new Transformer(new OrderedStreamFactory)
  
  val maxElToOutput = 20
  
  import lambda.Node._
  
  def interactivePause = {
    System.out.println("Press Any Key To Continue...");
    new java.util.Scanner(System.in).nextLine();
  }
  
  def assertWeight(lambdaNode: lambda.Node, weight: Float) =
    assertEquals(size(lambdaNode), weight, 0f)
    
  def assertWeight(expected: Int, weight: Float) =
    assertEquals(expected.toFloat, weight, 0f)	
    
  def assertWeight(pair: (lambda.Node, Float)) =
    assertEquals("Node " + pair._1, size(pair._1), pair._2, 0f)	    
    
  def assertTake(stream: Stream[(lambda.Node, Float)], num: Int) = {
    for (ind <- 0 until num) {
    	stream(ind)
//    	interactivePause
    }      
    val result = stream take num
    val message = "Part of the resulting stream: " + result.take(maxElToOutput).mkString("\n")
    
    for (ind <- 0 until result.size)
      assertWeight(result(ind))
    for (ind <- 0 until result.size - 1)
      assertTrue("Weight are not in non-decreasing order.\n" + "At position " + ind + "\n" + message, stream(ind)._2 <= stream(ind + 1)._2)
    result
  }
      
  @Test
  def treeReconstructBooleanToIntIntermediate: Unit = {  
  	// println(FormatSuccinctNode(exampleBoolToInt._1, 100))
    val extractorResults = assertTake(transformer(exampleBoolToInt._1), 1)
    
    assertEquals(1, extractorResults.size)
    assertEquals(transformer(exampleBoolToInt._1), extractorResults)
    
    for ( ((node, weight), lambdaNode) <- extractorResults zip constructBooleanToIntIntermediateLambda ) {
	    assertEquals(lambdaNode, node)	    
	    
	    assertWeight(lambdaNode, weight)	  
    }
  }
  
  @Test
  def treeIntToIntRec: Unit = {
    val intermediateTree = exampleIntToInt
            
    def confirmResults(num: Int) = {
	    val extractorResults = assertTake(transformer(intermediateTree), num)   
	    assertEquals(num, extractorResults.size)
	    
	    for ( ((node, _), lambdaNode) <- extractorResults zip constructIntToIntIntermediateFirstLambda(num))		  
		    assertEquals(node, lambdaNode)	    
    }
    
    for (ind <- 1 to 5) confirmResults(ind)    
  }
    
  @Test
  def treeIntAndBoolToIntIntermediate = {
    val intermediateTree = exampleIntToIntBoth
            
    def confirmResults(num: Int) = {
      // take two times this number of elements because we have two roots of recursion
      // take two times more to be sure that extractor extracts needed trees (node the non-determinism)
	    val extractorResults = assertTake(transformer(intermediateTree), (num * 2 * 2)) map { _._1 }	    
	    assertEquals(num * 4, extractorResults.size)
	    	  
	    val message = "Extracted " + extractorResults.zipWithIndex.map(p => p._2 + ": " + p._1).mkString("\n")
	    
	    for (node <- constructIntAndBoolToIntIntermediateLambda(num))
	    	assertTrue(node + " is not extracted.\n" + message, extractorResults contains node)	    
    }
    
    for (ind <- 1 to 5) confirmResults(ind)    
  }

}