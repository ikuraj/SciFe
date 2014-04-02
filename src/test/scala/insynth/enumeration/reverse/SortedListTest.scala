package insynth.enumeration
package reverse

import insynth.{ enumeration => e }
import reverse.{ dependent => rd }
import memoization._
import e.dependent._

import util._
import insynth.util.logging._
import insynth.util._

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._
import org.scalacheck.Gen

import scala.language.existentials

class SortedListTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {
  import Checks._
  import Structures._
  import Util._

  def tests(lists: Depend[(Int, Int), List[Int]]) {
    var elements: Iterable[List[Int]] = null
    def clue = elements.mkString("\n")
    
    withLazyClue("Elements are: " + clue) {
    
	    {
	      val en = lists.getEnum(1, 2)
	      elements = for (ind <- 0 until en.size) yield en(ind)
	      en.size should be (2)
	    }
    
	    {
	      val en = lists.getEnum(2, 3)
	      elements = for (ind <- 0 until en.size) yield en(ind)
	      en.size should be (3)
	    }
    
	    for (size <- 1 to 10) {
	      val en = lists.getEnum(size, size + 1)
	      elements = for (ind <- 0 until en.size) yield en(ind)
	      elements.size should be ( Binomial.binomialCoefficient(size + 1, size) )
	    }
    
      forAll( Gen.choose(1, 20), Gen.choose(1, 20), maxSize(50) ) { (size: Int, m: Int) =>
        whenever(true) {        
          elements = lists.getEnum((size, m)).toList
          
          forAll (elements) { lst =>
            forAll(lst.zip(lst.tail)) {
              case ((i, j)) => 
                assert( i > j )
            }
          }
        }
      }
    }
    
  }

  def constructEnumerator(ms: MemoizationScope = null) = {

    val naturals = rd.Reverser((range: Int) => { Reverser( 1 to range toList: _* ) })

    rd.Reverser(
      (self: rd.DependReverse[(Int, Int), List[Int]], pair: (Int, Int)) => {
        val (size, max) = pair

        if (size == 0) Reverser(Nil): Reverse[List[Int]]
        else if (size > 0) {
          val roots: Reverse[List[Int]] = naturals.getEnum(max)

          val innerLists: Depend[Int, List[Int]] = new InMap(self, { (par: Int) =>
            (size - 1, par)
          })

          val allLists =
            new dependent.ChainFinite(roots, innerLists)
                
//              (head: Int, l: List[Int]) => {
//                head :: l
//              })

          allLists
        } else e.Empty
      })
  }

//  test("enumerator regular enumeration") {
//    tests(constructEnumerator)
//  }
//
//  test("enumerator reverse enumeration") {
//    tests(constructEnumerator)
//  }

}
