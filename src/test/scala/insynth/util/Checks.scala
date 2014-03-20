package insynth.util

import org.scalatest._
import org.scalatest.Tag

import insynth.streams.light

object FocusTag extends Tag("focus")

object Checks {

  def noRepeat[T](elements: Traversable[T]) = {
    var enumerated: Set[T] = Set()

    (for (el <- elements) yield {
      val res = enumerated contains el
      enumerated ++= Set(el)
      !res
    }) forall identity

  }

  def nonDecreasing[T](elements: Iterable[(T, Int)]) = {
    (elements zip elements.tail) forall {
      case ((_, f), (_, s)) => f <= s
    }
  }

  // why is clue evaluated eagerly?
//  class Clueful[T](fun: => T) {
    def withLazyClue[T](clue: => Any)(fun: => T): T = {
      def append(currentMessage: Option[String]) =
        currentMessage match {
          case Some(msg) =>
            // clue.toString.head is guaranteed to work, because append() only called if clue.toString != ""
            val firstChar = clue.toString.head
            if (firstChar.isWhitespace || firstChar == '.' || firstChar == ',' || firstChar == ';')
              Some(msg + clue.toString)
            else
              Some(msg + " " + clue.toString)
          case None => Some(clue.toString)
        }
      try {
        val outcome = fun
        outcome match {
          case Failed(e: org.scalatest.exceptions.ModifiableMessage[_]) =>
            if (clue.toString != "")
            	Failed(e.modifyMessage(append)).asInstanceOf[T]
            else outcome
          case Canceled(e: org.scalatest.exceptions.ModifiableMessage[_]) =>
            if (clue.toString != "")
            	Canceled(e.modifyMessage(append)).asInstanceOf[T]
            else outcome
          case _ => outcome
        }
      } catch {
        case e: ModifiableMessage[_] =>
          if (clue.toString != "")
            throw e.modifyMessage(append)
          else
            throw e
      }
    }
//  }

  import scala.language.implicitConversions

  /**
   * Implicit conversion that allows clues to be place after a block of code.
   */
//  implicit def convertToClueful[T](fun: => T) = new Clueful(fun)

    class CheckerHelper[T] {
      var _res: light.Enum[T] = null
      var elements: Seq[T] = null
      def res = _res
      def res_=(n: light.Enum[T]) = {
        _res = n
        elements = (0 until res.size) map { res(_) }
      }
      var addMessage: String = ""
      def clue =
        (0 until res.size).map(res(_)).mkString("\n") + "\n" + addMessage
    }
}

class ChecksTest extends FunSuite with ShouldMatchers {

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