package insynth.streams
package light

import scala.language.implicitConversions

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class CombinatorsTest extends FunSuite with ShouldMatchers {
  
  import Enum._
  
  test("simple") {
    val listEnum: Enum[(Int, Int)] = List( (1, 1), (3, 3) )
    val rangeEnum: Enum[Int] = (1 to 9)
    val streamEnum: Enum[Int] = Stream.from(0)
    
    val allConvenientlyDivisblePairs =
			( listEnum ++
			(rangeEnum ** streamEnum)
			) map { case (x, y) => ("valid", x, y) }
    
  }

}