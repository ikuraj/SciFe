package insynth.common

import insynth.reconstruction.stream.Node

import org.junit.Assert._

object CommonUtils {
  import Node._

  type Weight = Int
  
  type Output = (Node, Int)
  
  val maxElToOutput = 20
  
  def interactivePause = {
    System.out.println("Press Any Key To Continue...");
    new java.util.Scanner(System.in).nextLine();
  }
      
  def assertWeight(lambdaNode: Node, weight: Weight) =
    assertEquals(size(lambdaNode), weight)
    
  def assertWeight(expected: Int, weight: Weight) =
    assertEquals(expected, weight)  
    
  def assertWeight(pair: (Node, Weight)) =
    assertEquals("Node " + pair._1, size(pair._1), pair._2)     
    
  def assertTake(stream: Stream[(Node, Weight)], num: Int) = {
    val result = stream take num
    val message = "Part of the resulting stream: " + result.take(maxElToOutput).mkString("\n")
    
    for (ind <- 0 until result.size)
      assertWeight(result(ind))
    for (ind <- 0 until result.size - 1)
      assertTrue("Weight are not in non-decreasing order.\n" + "At position " + ind + "\n" + message, stream(ind)._2 <= stream(ind + 1)._2)
    result
  }

}