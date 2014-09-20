package scife.enumeration
package reverse

import scife.{ enumeration => e }
import reverse.{ dependent => rd }
import memoization._
import e.dependent._

import util._
import scife.util.logging._
import scife.util._

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._
import org.scalacheck.Gen

import scala.language.existentials
import scala.language.postfixOps

class SortedListTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {
  import Checks._
  import Structures._
  import Util._

  def constructEnumerator(ms: MemoizationScope = null): rd.DependReverse[(Int, Int), List[Int]] = {

    val naturals = rd.Reverser((range: Int) => { Reverser(1 to range toList) })

    rd.Reverser(
      (self: e.dependent.Depend[(Int, Int), List[Int]], pair: (Int, Int)) => {
        val (size, max) = pair

        if (size == 0) Reverser(List.empty[Int]): Reverse[List[Int]]
        else if (size > 0) {
          val roots: Reverse[Int] = naturals.getEnum(max)

          val innerLists: rd.DependReverse[Int, List[Int]] = rd.InMap(
            // XXX HACK!
            self.asInstanceOf[rd.DependReverse[(Int, Int), List[Int]]], { (par: Int) =>
              (size - 1, par)
            })

          val allLists: Reverse[(Int, List[Int])] =
            new rd.ChainFinite(roots, innerLists)

          val makeList =
            (p: (Int, List[Int])) => {
              p._1 :: p._2
            }

          val reverseList =
            (list: List[Int]) => {
              (list.head, list.tail)
            }

          new e.reverse.Map[(Int, List[Int]), List[Int]](allLists, makeList, reverseList): Reverse[List[Int]]
        } else Empty: Reverse[List[Int]]
      })
  }

  test("enumerator regular enumeration") {
    lists = constructEnumerator()
    checks
    checkInvariant
  }

  test("enumerator reverse enumeration") {
    lists = constructEnumerator()
    withLazyClue("Elements are: " + clue) {
      {
        val en = lists.getEnum(1, 2): Reverse[List[Int]]
        for (list <- List(List(1), List(2))) {
          en.reverse(list) should be(list.head - 1)
        }
      }

      {
        val en = lists.getEnum(2, 2): Reverse[List[Int]]
        elements = for (ind <- 0 until en.size) yield en(ind)
        for (ind <- 0 until elements.size) {
          val elToReverse = elements(ind)
          val newInd = en.reverse(elToReverse)
          val newElements = for (innerIn <- newInd until en.size) yield en(innerIn)

          newElements should contain theSameElementsAs (elements.drop(ind))
        }
      }

      forAll(Gen.choose(1, 5), Gen.choose(1, 5), maxSize(50)) { (size: Int, m: Int) =>
        {
          val en = lists.getEnum(size, m): Reverse[List[Int]]
          elements = for (ind <- 0 until en.size) yield en(ind)
          for (ind <- 0 until elements.size) {
            val elToReverse = elements(ind)
            val newInd = en.reverse(elToReverse)
            val newElements = for (innerIn <- newInd until en.size) yield en(innerIn)

            newElements should contain theSameElementsAs (elements.drop(ind))
          }
        }
      }
    }
  }

  def checkInvariant =
    // keep this simple - we are not using memoization in tests
    forAll(Gen.choose(1, 5), Gen.choose(1, 5), maxSize(50)) { (size: Int, m: Int) =>
      //      finest("size =%d, m=%d".format(size: Int, m: Int))
      whenever(true) {
        elements = lists.getEnum((size, m)).toList

        for (lst <- elements; (i, j) <- lst.zip(lst.tail)) {
          assert(i >= j)
        }
      }
    }

  def checks {
    withLazyClue("Elements are: " + clue) {
      {
        val en = lists.getEnum(1, 2)
        elements = for (ind <- 0 until en.size) yield en(ind)
        en.size should be(2)
      }

      {
        val en = lists.getEnum(2, 2)
        elements = for (ind <- 0 until en.size) yield en(ind)
        en.size should be(3)
      }

      {
        val en = lists.getEnum(2, 3)
        elements = for (ind <- 0 until en.size) yield en(ind)
        en.size should be(6)
      }
    }
  }

  var elements: Seq[List[Int]] = null
  def clue = elements.mkString("\n")
  var lists: rd.DependReverse[(Int, Int), List[Int]] = _

}
