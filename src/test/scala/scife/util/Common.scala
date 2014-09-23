package scife.util

import scala.language.implicitConversions
import scala.language.postfixOps

object Common {
  
  implicit def flatten1[A, B, C](t: ((A, B), C)): (A, B, C) = (t._1._1, t._1._2, t._2)
  implicit def flatten2[A, B, C](t: (A, (B, C))): (A, B, C) = (t._1, t._2._1, t._2._2)
  implicit def flatten3[A, B, C, D](t: ((A, B), (C, D))): (A, B, C, D) = (t._1._1, t._1._2, t._2._1, t._2._2)
  implicit def flatten4[A, B, C, D](t: (((A, B), C), D)): (A, B, C, D) = (t._1, t._2)
  
  val fromOne = Stream.from(1)
  val ones = Stream.continually(1)

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