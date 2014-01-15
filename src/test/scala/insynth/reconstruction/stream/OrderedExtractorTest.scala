package insynth.reconstruction

import insynth.structures.{ Function => FunctionType, _ }
import insynth.reconstruction.{ stream => lambda }
import insynth.reconstruction.stream._

import org.scalatest.junit.JUnitSuite
import org.junit.{ Test, Ignore }
import org.junit.Assert._

import insynth.common._
import insynth.testdomain.{ TestDeclaration => Declaration }

class OrderedExtractorTest extends JUnitSuite {

  import CommonDeclarations._
  import CommonLambda._
  import CommonProofTrees._
  import CommonUtils._
    
  val extractor = new Transformer(new OrderedStreamFactory)
  
  val maxElToOutput = 20
  
  import lambda.Node._
      
  @Test
  def treeReconstructBooleanToIntIntermediate: Unit = {  
    val extractorResults = assertTake(extractor(exampleBoolToInt._1), 1)
    
    assertEquals(1, extractorResults.size)
    
    for ( ((node, weight), lambdaNode) <- extractorResults zip constructBooleanToIntIntermediateLambda ) {
	    assertEquals(lambdaNode, node)	    
	    
	    assertWeight(lambdaNode, weight)	  
    }
  }
  
//  @Test
//  def treeIntToIntRec: Unit = {
//    
//    val intermediateTree = exampleIntToInt
//            
//    def confirmResults(num: Int) = {
//	    val extractorResults = assertTake(extractor(intermediateTree), num)   
//	    assertEquals(num, extractorResults.size)
//	    
//	    for ( ((node, _), lambdaNode) <- extractorResults zip constructIntToIntIntermediateFirstLambda(num))		  
//		    assertEquals(node, lambdaNode)	    
//    }
//    
//    for (ind <- 1 to 5) confirmResults(ind)    
//  }
//    
//  @Test
//  def treeIntAndBoolToIntIntermediate = {
//    val intermediateTree = exampleIntToIntBoth
//            
//    def confirmResults(num: Int) = {
//      // take two times this number of elements because we have two roots of recursion
//      // take two times more to be sure that extractor extracts needed trees (node the non-determinism)
//	    val extractorResults = assertTake(extractor(intermediateTree), (num * 2 * 2)) map { _._1 }	    
//	    assertEquals(num * 4, extractorResults.size)
//	    	  
//	    val message = "Extracted " + extractorResults.zipWithIndex.map(p => p._2 + ": " + p._1).mkString("\n")
//	    
//	    for (node <- constructIntAndBoolToIntIntermediateLambda(num))
//	    	assertTrue(node + " is not extracted.\n" + message, extractorResults contains node)	    
//    }
//    
//    for (ind <- 1 to 5) confirmResults(ind)    
//  }

}