package insynth.util

import org.scalatest._
import org.scalatest.matchers._

object Checks {

  def noRepeat[T](elements: Traversable[T]) = {
    var enumerated: Set[T] = Set()
    
    (for(el <- elements) yield {
      val res = enumerated contains el
      enumerated ++= Set(el)        
      !res    
    }) forall identity
    
  }

  def nonDecreasing[T](elements: Iterable[(T, Float)]) = {
    (elements zip elements.tail) forall {
      case ((_, f), (_, s)) => f <= s
    }
  }
  
}

class ChecksTest extends FunSuite with ShouldMatchers {
  
  import Checks._
  
  test("no repeat with no repeats") {    
    noRepeat(1 to 100) should be (true)
  }

  test("no repeat with repeats") {    
    noRepeat((1 to 100) :+ 99) should be (false)

    noRepeat((1 to 100) :+ 100) should be (false)
    
    noRepeat((1 to 100) :+ 1) should be (false)
    
    noRepeat(1 :: (1 to 100).toList) should be (false)
  }
  
//  test("nonDecreasing true") {    
//    nonDecreasing(1 to 100 zip (1 to 100)) should be (true)
//  }
//
//  test("nonDecreasing false") {    
//    var floatList = List(1f)
//    var list: List[Int] = (1 to 100) :+ 99 
//    floatList = list map { _.toFloat }
//    nonDecreasing(floatList zip floatList) should be (false)
//  
//    list = (1 to 100) :+ 50
//    floatList = list map { _.toFloat }
//    nonDecreasing(floatList zip floatList) should be (false)
//      
//    list = (1 to 100) :+ 1
//    floatList = list map { _.toFloat }
//    nonDecreasing(floatList zip floatList) should be (false)
//    
//    list = (List(1) :: (2 to 50).toList :: List(52) :: List(51 to 100).toList).flatten
//    floatList = list map { _.toFloat }
//    nonDecreasing(floatList zip floatList) should be (false)
//  }
}