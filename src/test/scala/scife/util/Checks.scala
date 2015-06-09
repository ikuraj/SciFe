package scife.util

import org.scalatest._

object Checks {

  def noRepeat[T](elements: Traversable[T]) = {
    var enumerated: Set[T] = Set()

    (for (el <- elements) yield {
      val res = enumerated contains el
      enumerated ++= Set(el)
      !res
    }) forall identity

  }

  def nonDecreasing[T, U <% Ordered[U]](elements: Iterable[(T, U)]) = {
    (elements zip elements.tail) forall {
      case ((_, f), (_, s)) => f <= s
    }
  }

}

class ChecksTest extends FunSuite with Matchers {

  import Checks._

  test("no repeat with no repeats") {
    noRepeat(1 to 100) should be(true)
  }

  test("no repeat with repeats") {
    noRepeat((1 to 100) :+ 99) should be(false)

    noRepeat((1 to 100) :+ 100) should be(false)

    noRepeat((1 to 100) :+ 1) should be(false)

    noRepeat(1 :: (1 to 100).toList) should be(false)
  }

    test("nonDecreasing true") {
      nonDecreasing(1 to 10 zip (1 to 10)) should be (true)
    }
  
    test("nonDecreasing false") {
      val list = (1 to 10).toList ::: 9 :: Nil

      val l1 = list map { _.toFloat }
      nonDecreasing(l1 zip l1) should be (false)
  
      val l2 = (1 to 10).toList :+ 5
      nonDecreasing(l2 zip l2) should be (false)
  
      val l3 = (1 to 10).toList :+ 1
      nonDecreasing(l3 zip l3) should be (false)
  
      val l4 = (List(1) :: (2 to 5).toList :: List(7) :: List(6 to 10).toList).flatten
      nonDecreasing(l4 zip l4) should be (false)
    }
}
