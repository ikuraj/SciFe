package insynth
package enumeration

import org.scalatest._
import org.scalatest.Tag

object FocusTag extends Tag("focus")

object Checks {

  import scala.language.implicitConversions

  /**
   * Implicit conversion that allows clues to be place after a block of code.
   */
//  implicit def convertToClueful[T](fun: => T) = new Clueful(fun)

  class CheckerHelper[T] {
    var _res: Enum[T] = null
    var elements: Seq[T] = null
    def res = _res
    def res_=(n: Enum[T]) = {
      _res = n
      elements = (0 until res.size) map { res(_) }
    }
    var addMessage: String = ""
    def clue =
      (0 until res.size).map(res(_)).mkString("\n") + "\n" + addMessage
  }
}