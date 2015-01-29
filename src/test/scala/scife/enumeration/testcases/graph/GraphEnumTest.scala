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

class GraphEnumTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {

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

    val enum = graphEnum//constructEnumerator

    withLazyClue("Elements are: " + clue) {

      res = enum.getEnum(0)
      res.size should be(1)
      res(0) should be(Empty)

      res = enum.getEnum(1)
      res.size should be(1)
      res(0) should be((Nil, 1, Nil) &: Empty)

      res = enum.getEnum(2)
      res.size should be > (1)

      res = enum.getEnum(3)
      res.toList should contain(
        (List(1, 2), 3, List(2)) &:
          (Nil, 2, List(1)) &:
          (Nil, 1, Nil) &:
          Empty)
      // note the order of edges should be sorted
      res.toList should not contain(
        (List(2, 1), 3, List(2)) &:
          (Nil, 2, List(1)) &:
          (Nil, 1, Nil) &:
          Empty)
    }

  }

//  def constructEnumerator(implicit ms: MemoizationScope = null) = {
//
//    import Enum._
//
//    //    var result: DependFinite[Input, Output] = null
//    //
//    //    def toMax(maxOrdinal: Int, ordinal: Int) = {
//    //
//    //      if (ordinal > maxOrdinal) e.Empty
//    //      else {
//    //        val rest = result(ordinal + 1)
//    //        if (ordinal == 0) (Empty: Output) concat rest
//    //        else {
//    //          val sublistEnum = Sublists((1 to ordinal).toList)
//    //          val sublists = sublistEnum.product(sublistEnum)
//    //
//    //          Map.memoized(sublists.product(rest), { (in: ((List[Int], List[Int]), Graph)) =>
//    //            val ((left, right), restGraph) = in
//    //            (left, ordinal, right) &: restGraph
//    //          })
//    //          //        sublists.product(rest) map {
//    //          //          case ( (left, right), restGraph) =>
//    //          //            (left, ordinal, right) &: restGraph
//    //          //        }
//    //
//    //        }: Finite[Output]
//    //      }
//    //    }
//
//    //    result =
//    Depend.memoizedFin[Input, Output](
//      (self: DependFinite[Input, Output], maxOrdinal: Input) => {
//
//        def toMax(ordinal: Int) = {
//          val sublistEnum = Sublists((1 until ordinal).toList)
//          val sublists = sublistEnum.product(sublistEnum)
//
//          Map.memoized(sublists.product(self(ordinal - 1)), { (in: ((List[Int], List[Int]), Graph)) =>
//            val ((left, right), restGraph) = in
//            (left, ordinal, right) &: restGraph
//          }) //        sublists.product(rest) map {
//          //          case ( (left, right), restGraph) =>
//          //            (left, ordinal, right) &: restGraph
//          //        }
//          : Finite[Output]
//        }
//
//        if (maxOrdinal == 0) Singleton(Empty)
//        else toMax(maxOrdinal)
//      })
//
//    //    result
//  }

}
