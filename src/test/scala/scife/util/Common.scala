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
            for (
              el <- integers;
              recList <- rec(sizeToGen - 1)
            ) yield el :: recList

          result.toList
        }
      }

    rec(maxSize)
  }

  import org.scalatest._

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

}