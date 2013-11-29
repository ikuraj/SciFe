package insynth.util

import org.scalatest._
import org.scalatest.matchers._

object Structures {

  def generateLists(maxSize: Int, integers: Range) = {
    def rec(sizeToGen: Int): List[List[Int]] = {
      if (sizeToGen == 0) List(Nil)
      else {
        val result =
          for ( el <- integers; 
            recList <- rec(sizeToGen - 1))
            yield el :: recList
          
        result.toList
      }
    }
    
    rec(maxSize)
  }
  
}

class StructuresTest extends FunSuite with ShouldMatchers {
  
  import Structures._
  
  test("generate lists") {
    val lists = generateLists(3, 1 to 3)
    
    lists.size should be (3 * 3 * 3)
  }
}