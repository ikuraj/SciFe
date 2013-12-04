package insynth.util

import insynth.attrgrammar._
import insynth.reconstruction.stream._

import org.scalatest._
import org.scalatest.matchers._

import scala.language.implicitConversions

object Structures {
  
  import StreamableAST._

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

  object Binomial {
    def main(args: Array[String]): Unit = {
      val n = 5
      val k = 3
      val result = binomialCoefficient(n, k)
      println("The Binomial Coefficient of %d and %d equals %d.".format(n, k, result))
    }

    def binomialCoefficient(n: Int, k: Int): Int = 
      ( (BigInt(n - k + 1) to n).product / 
      (BigInt(1) to k).product).intValue
  }
  
  case class JustInt(a: Int)
  trait CusList
  case object CusNil extends CusList
  case class Cons(e: JustInt, list: CusList) extends CusList
  
  object CusList {
    def isSorted(list: CusList) = {
      def rec(l: CusList): Boolean = l match {
        case Cons(JustInt(a), innerList@Cons(JustInt(b), _)) =>
          a <= b && rec(innerList)
        case _ => true  
      }
      
      rec(list)
    }
    
    def size(list: CusList) = {
      def rec(l: CusList): Int = l match {
        case CusNil => 0
        case Cons(el, inList) => 1 + rec(inList)
      }
      
      rec(list)
    }
    
    implicit def toCusList(list: List[Int]) = {
      def rec(l: List[Int]): CusList = l match {
        case Nil => CusNil
        case el :: inList =>
          Cons(JustInt(el), rec(inList))
      }
      
      rec(list)
    }
  }
  
}

class StructuresTest extends FunSuite with ShouldMatchers {
  
  import Structures._
  
  test("generate lists") {
    {
      val lists = generateLists(3, 1 to 3)
      
      lists.size should be (3 * 3 * 3)
    }
    
    {
      val lists = generateLists(1, 1 to 1)
      
      lists.size should be (1)
    }    
    
    {
      val lists = generateLists(2, 1 to 1)
      
      lists.size should be (1)
    }    
  }
  
  import CusList._ 
  
  test("isSorted") {
    
    for(ex <- List(
      List(1, 2, 2),
      List(1, 2),
      List(1, 1),
      List(1, 1, 2, 2, 3),
      List(1, 1, 1, 1, 1, 2, 3)
    ): List[CusList])
      isSorted(ex) should be (true)
    
    for(ex <- List(
      List(1, 3, 2),
      List(3, 2),
      List(3, 1),
      List(1, 1, 2, 2, 1),
      List(1, 1, 1, 1, 1, 2, 1)
    ): List[CusList])
      isSorted(ex) should be (false)
  }
}