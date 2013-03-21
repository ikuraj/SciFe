package insynth.common

import insynth.reconstruction.stream.Node

import org.junit.Assert._

object CommonUtils {
  
  type Output = (Node, Float)
  
  val maxElToOutput = 20
      
  def assertTake(stream: Stream[Output], num: Int) = {
    val result = stream take num
    val message = "Part of the resulting stream: " + result.take(maxElToOutput).mkString("\n")
    
    for (ind <- 0 until result.size - 1)
      assertTrue("Weight are not in non-decreasing order.\n" + "At position " +
      ind + "\n" + message, stream(ind)._2 <= stream(ind + 1)._2)
    result
  }

}