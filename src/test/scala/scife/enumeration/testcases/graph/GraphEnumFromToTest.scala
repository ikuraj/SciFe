package scife.enumeration
package testcases
package graph

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._

import dependent._
import scife.{ enumeration => e }
import memoization._

import scife.util._
import scife.util.logging._
import structures.BSTrees._

import org.scalatest._
import org.scalameter.api._

import scala.language.postfixOps
import scala.language.existentials

class GraphEnumFromToTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {

  import Checks._
  import Util._
  import Graph._

  import GraphEnum._

  // (size, available, declared)
  type Input = Int
  // list of extends
  type Output = Graph
  type EnumType = Depend[Input, Output]

  test("enumeration") {
    val checkerHelper = new CheckerHelper[Output]
    import checkerHelper._

    withLazyClue("Elements are: " + clue) {

      res = graphEnumFromToMax(0, 0)
      res.size should be(1)
      res(0) should be(Empty)

      res = graphEnumFromToMax(0, 1)
      res.size should be(1)
      res(0) should be((Nil, 1, Nil) &: Empty)

      res = graphEnumFromToMax(1, 1)
      res.size should be (4)
      res.toList should contain allOf (
        (Nil, 2, Nil) &: Empty,
        (1 :: Nil, 2, Nil) &: Empty,
        (Nil, 2, 1 :: Nil) &: Empty,
        (1 :: Nil, 2, 1 :: Nil) &: Empty
      )

      res = graphEnumFromToMax(3, 2)
      res.toList should contain allOf (
        (Nil, 4, List(1)) &: (Nil, 5, Nil) &: Empty,
        (Nil, 4, List(1)) &: (1 :: 2 :: Nil, 5, 3 :: Nil) &: Empty,
        (Nil, 4, List(1)) &: (1 :: 2 :: Nil, 5, 3 :: 4 :: Nil) &: Empty
      )
      // note the order of edges should be sorted
      res.toList should not contain(
        (Nil, 2, List(1)) &:
        (Nil, 1, Nil) &:
        Empty
      )
    }

  }

}
