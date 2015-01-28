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

class WithGraphEnumTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {

  import Checks._
  import Util._
  import Graph._
  
  import GraphEnum._
  
  type Input = (Int, Graph)
  type Output = Graph
  type EnumType = Depend[Input, Output]

  test("enumeration") {
    val checkerHelper = new CheckerHelper[Output]
    import checkerHelper._

    val enum = constructEnumerator
    var inputGraph: Graph = null

    withLazyClue(s"Input graph is $inputGraph and elements are $clue") {
      
      inputGraph = Empty

      res = enum.getEnum(0, inputGraph)
      res.size should be(1)
      res(0) should be(Empty)

      res = enum.getEnum(1, inputGraph)
      res.size should be(1)

      res = enum.getEnum(2, inputGraph)
      res.size should be(4)

      res = enum.getEnum(3, inputGraph)
      res.size should be(64)
      
      inputGraph = (Nil, 1, Nil) &:
        Empty
        
      // just to make sure
      inputGraph.size should be (1)
      val gEnum = GraphEnum.graphEnum 
      gEnum(0) shouldBe Singleton(Empty)

      res = enum.getEnum(1, inputGraph)
      res.size should be(1)
      res(0) should be(inputGraph)

      res = enum.getEnum(2, inputGraph)
      res.size should be(4)

//      
//      inputGraph = (Nil, 2, List(1)) &:
//        (Nil, 1, Nil) &:
//        Empty
//
//      res = enum.getEnum(inputGraph)
//      res.size should be(4)
//      res.toList should contain allOf(
//        inputGraph,
//        (List(1), 2, List(1)) &: (Nil, 1, Nil) &: Empty
//      )
//      
//      inputGraph = (List(1, 2), 3, List(2)) &:
//        (Nil, 2, List(1)) &:
//        (Nil, 1, Nil) &:
//        Empty
//
//      res = enum.getEnum(inputGraph)
//      res.size should be > (4)
//      res.toList should contain allOf(
//        inputGraph,
//        (List(1, 2), 3, List(2)) &:
//        (List(1), 2, List(1)) &:
//        (Nil, 1, Nil) &:
//        Empty,
//        (List(1, 2), 3, List(2, 1)) &:
//        (Nil, 2, List(1)) &:
//        (Nil, 1, Nil) &:
//        Empty
//      )
    }

  }

  def constructEnumerator(implicit ms: MemoizationScope = null) = {

    import Enum._
    
    val graphs = GraphEnum.graphEnum
    val wrapSubgraph = wrapSubgraphEnum 

    Depend.memoizedFin[Input, Output](
      (self: DependFinite[Input, Output], pair: Input) => {
        val (size, graph) = pair
        
        def wrapSubgraphAtRoot(implicit ms: MemoizationScope = null): OverrideFunctionFin[Graph, Output] = {
          new OverrideFunctionFin[Graph, Output](
            (self: Graph => Finite[Output], currGraph: Graph) => {
              if (currGraph == Graph.empty) 
                generate i-th to size layer of general graph, then decrement 
                graphs(size - graph.size)
              else self(currGraph)
            },
            wrapSubgraph)
        }
        
        val wrapSubgraphRes = wrapSubgraphAtRoot(ms)
        
        wrapSubgraphRes(graph)
      })

  }

}
