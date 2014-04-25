package insynth
package enumeration

import org.scalatest._
import org.scalatest.Tag

object FocusTag extends Tag("focus")

object Util {

  import scala.language.implicitConversions

  /**
   * Implicit conversion that allows clues to be place after a block of code.
   */
//  implicit def convertToClueful[T](fun: => T) = new Clueful(fun)

  class CheckerHelper[T] {
    var _res: Enum[T] = null
    var _elements: Seq[T] = Seq()
    def elements = _elements
    def elements_=(n: Seq[T]) = _elements = n
    def res = _res
    def res_=(n: Enum[T]) = {
      _res = n
      elements = (0 until res.size) map { res(_) }
    }
    var addMessage: String = ""
    def clue =
      if (res == null) elements.mkString("\n") + "\n" + addMessage
      else (0 until res.size).map(res(_)).mkString("\n") + "\n" + addMessage
  }

  class CheckerHelperFun[T]( fun: (Seq[T] => _) ) extends CheckerHelper[T] {
    override def elements_=(n: Seq[T]) = {
      super.elements_=(n)
      fun(n)
    }
  }
}