package insynth.reconstruction.stream

import org.scalatest.junit.JUnitSuite
import org.junit.{ Test, Ignore }
import org.junit.Assert._

import insynth.common.{ CommonDeclarations, CommonLambda }

class LambdaTreesTest extends JUnitSuite {

  import Node._
  import CommonLambda._
    
  @Test
  def testIdent = {
    val node = booleanIdentifier
    
    assertEquals(1, size(node))
  }
  
  @Test
  def testSimple = {
    val node = constructBooleanToIntIntermediateLambda.head
    
    assertEquals(3, size(node))
    
    node match {
      case Application(_, query :: List(innerApp)) =>
      	assertEquals(2, size(innerApp))
      case _ => fail
    }
  }
  
  @Test
  def testRecursive = {
    val nodes = constructIntToIntIntermediateFirstLambda(5)
    
    for ((node, s) <- nodes zip List(3, 4, 5, 6, 7) ) {
      val message = node + " do not have the given size"
      
	    assertEquals(message, s, size(node))
	    
	    node match {
	      case Application(_, query :: List(innerApp)) =>
	      	assertEquals(message, s - 1, size(innerApp))
	      case _ => fail
	    }
    }
  }
  
}