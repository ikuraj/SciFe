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
import Structures.BSTrees._

import org.scalatest._
import org.scalameter.api._

import scala.language.postfixOps
import scala.language.existentials

object GraphEnum extends HasLogger with ProfileLogger {

  import Util._
  import Graph._

  def wrapSubgraphEnum(implicit ms: MemoizationScope = null) = {

    type Input = Graph
    type Output = Graph
    type EnumType = Depend[Input, Output]

    import Enum._

    Depend.memoizedFin[Input, Output](
      (self: DependFinite[Input, Output], subGraph: Input) => {

        subGraph match {
          case Empty => Singleton(Empty)
          case (left, ordinal, right) &: restSubGraph =>
            val sublistEnum = Sublists((1 until ordinal).toList)
            val sublists = sublistEnum.product(sublistEnum)

            val currNodes = sublists map {
              pair => (left ++ pair._1, ordinal, right ++ pair._2)
            }

            self(restSubGraph) product currNodes map {
              case (restGraph, currNode) => currNode &: restGraph
            }
        }

      })

  }
  
  val sublistEnum = Depend memoizedFin { (ordinal: Int) =>
    Sublists((1 to ordinal).toList)
  }
  val sublists = sublistEnum.product(sublistEnum)

  val graphOfOrdinal =
    Depend memoizedFin {
      (self: DependFinite[Int, Context], ordinal: Int) =>

        Map.memoized(sublists(ordinal - 1),
          { (in: (List[Int], List[Int])) =>
            val (left, right) = in
            (left, ordinal, right)
          }): Finite[Context]
    }

  def graphEnumFromToMax(existing: Int, size: Int)
    (implicit ms: MemoizationScope = null): Finite[Graph] = {

    import Enum._

    if (size == 0) Singleton(Empty)
    else {
      Map.memoized(
        graphOfOrdinal(existing + 1) product graphEnumFromToMax(existing + 1, size - 1),
          { (in: (Context, Graph)) =>
              val (ctx, restGraph) = in
              ctx &: restGraph
            }): Finite[Graph]
    }

  }

  def graphEnum(implicit ms: MemoizationScope = null) = {

    type Input = Int
    type Output = Graph
    type EnumType = Depend[Input, Output]

    import Enum._

    //    var result: DependFinite[Input, Output] = null
    //
    //    def toMax(maxOrdinal: Int, ordinal: Int) = {
    //
    //      if (ordinal > maxOrdinal) e.Empty
    //      else {
    //        val rest = result(ordinal + 1)
    //        if (ordinal == 0) (Empty: Output) concat rest
    //        else {
    //          val sublistEnum = Sublists((1 to ordinal).toList)
    //          val sublists = sublistEnum.product(sublistEnum)
    //
    //          Map.memoized(sublists.product(rest), { (in: ((List[Int], List[Int]), Graph)) =>
    //            val ((left, right), restGraph) = in
    //            (left, ordinal, right) &: restGraph
    //          })
    //          //        sublists.product(rest) map {
    //          //          case ( (left, right), restGraph) =>
    //          //            (left, ordinal, right) &: restGraph
    //          //        }
    //
    //        }: Finite[Output]
    //      }
    //    }

    //    result =
    Depend.memoizedFin[Input, Output](
      (self: DependFinite[Input, Output], maxOrdinal: Input) => {

          def toMax(ordinal: Int) = {
            val sublistEnum = Sublists((1 until ordinal).toList)
            val sublists = sublistEnum.product(sublistEnum)

            Map.memoized(sublists.product(self(ordinal - 1)), { (in: ((List[Int], List[Int]), Graph)) =>
              val ((left, right), restGraph) = in
              (left, ordinal, right) &: restGraph
            }) //        sublists.product(rest) map {
            //          case ( (left, right), restGraph) =>
            //            (left, ordinal, right) &: restGraph
            //        }
            : Finite[Output]
          }

        if (maxOrdinal == 0) Singleton(Empty)
        else toMax(maxOrdinal)
      })

    //    result
  }

}
