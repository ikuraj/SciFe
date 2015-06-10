package scife
package enumeration
package showcase

import dependent._
import memoization._
import scife.{ enumeration => e }
import scife.util._

import scife.util.logging._

import structures._
import BSTrees._

import org.scalatest._
import org.scalatest.prop._
import org.scalameter.api._

import scala.language.existentials
import scala.language.implicitConversions

class BinarySearchTreeMonad extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {

  import Util.CheckerHelper
  import Checks._
  import Math._
  import Common._

  // DSL
  import e._
  import Enum._
  import Depend._

  test("Correctness of variants of BST enumerator") {
    implicit val ms = new scope.AccumulatingScope

    val helper = new CheckerHelper[Tree]
    import helper._

    for (enum <- List(constructorMonad)) {
      withLazyClue("Elements are: " + clue) {
        res = enum.getEnum(1, 1 to 3)
        res.size should be (3)
        elements should contain theSameElementsAs (1 to 3).map(
          Node(Leaf, _, Leaf))

        res = enum.getEnum(2, 1 to 2)
        res.size should be (2)
        elements should contain allOf (
          Node(Leaf, 1, Node(Leaf, 2, Leaf)),
          Node(Node(Leaf, 1, Leaf), 2, Leaf))

        res = enum.getEnum(3, 1 to 3)
        res.size should be (5)
        elements should contain allOf (
          Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
          Node(Leaf, 1, Node(Node(Leaf, 2, Leaf), 3, Leaf)))

        res = enum.getEnum(3, 1 to 4)
        res.size should be (5 * Binomial.binomialCoefficient(4, 3))
        elements should contain allOf (
          Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)),
          Node(Leaf, 1, Node(Node(Leaf, 2, Leaf), 3, Leaf)))

        for (size <- 1 to 3) {
          res = enum.getEnum((size, size to size - 1))
          res.size should be (0)
          elements should be ('empty)

          res = enum.getEnum((0, 1 to size))
          res(0) should be (Leaf)
          res.size should be (1)
        }

        // some confirmed counts
        res = enum.getEnum(7, 1 to 7)
        res.size should be (429)

      }

    }
  }

  def constructorMonad(implicit ms: MemoizationScope) = {
    val res =
      rec[(Int, Range), Tree]({
        case (self, (size, r)) => {

          if (size <= 0) Leaf
          else {
            Enum((for (m <- r;
                lr = r.start to (m-1);
                rr = m+1 to r.end;
                ls <- 0 until size;
                rs = size - ls - 1;
                lt <- self(ls, lr);
                rt <- self(rs, rr)) yield 
                  Node(lt, m, rt)).toArray)
          }
        }
      })

    res
  }

}
