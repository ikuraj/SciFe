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

class WrapGraphEnumTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {

  import Checks._
  import Util._
  import Graph._
  import Common._
  
  import GraphEnum._

  type Input = Graph
  type Output = Graph
  type EnumType = Depend[Input, Output]

  test("enumeration") {
    val checkerHelper = new CheckerHelper[Output]
    import checkerHelper._

    val enum = wrapSubgraphEnum//constructEnumerator
    var inputGraph: Graph = null

    withLazyClue(s"Input graph is $inputGraph and elements are $clue") {
      
      inputGraph = Empty

      res = enum.getEnum(inputGraph)
      res.size should be(1)
      res(0) should be(Empty)
      
      inputGraph = (Nil, 1, Nil) &:
        Empty

      res = enum.getEnum(inputGraph)
      res.size should be(1)
      res(0) should be(inputGraph)
      
      inputGraph = (Nil, 2, List(1)) &:
        (Nil, 1, Nil) &:
        Empty

      res = enum.getEnum(inputGraph)
      res.size should be(4)
      res.toList should contain allOf(
        inputGraph,
        (List(1), 2, List(1)) &: (Nil, 1, Nil) &: Empty
      )
      
      inputGraph = (List(1, 2), 3, List(2)) &:
        (Nil, 2, List(1)) &:
        (Nil, 1, Nil) &:
        Empty

      res = enum.getEnum(inputGraph)
      res.size should be > (4)
      res.toList should contain allOf(
        inputGraph,
        (List(1, 2), 3, List(2)) &:
        (List(1), 2, List(1)) &:
        (Nil, 1, Nil) &:
        Empty,
        (List(1, 2), 3, List(2, 1)) &:
        (Nil, 2, List(1)) &:
        (Nil, 1, Nil) &:
        Empty
      )
    }

  }

//  def constructEnumerator(implicit ms: MemoizationScope = null) = {
//
//    import Enum._
//
//    Depend.memoizedFin[Input, Output](
//      (self: DependFinite[Input, Output], subGraph: Input) => {
//        
//        subGraph match {
//          case Empty => Singleton(Empty)
//          case (left, ordinal, right) &: restSubGraph =>
//            val sublistEnum = Sublists((1 until ordinal).toList)
//            val sublists = sublistEnum.product(sublistEnum)
//            
//            val currNodes = sublists map {
//              pair => (left ++ pair._1, ordinal, right ++ pair._2)
//            }
//            
//            self(restSubGraph) product currNodes map {
//              case (restGraph, currNode) => currNode &: restGraph
//            }
//        }
//
//      })
//
//  }

}
