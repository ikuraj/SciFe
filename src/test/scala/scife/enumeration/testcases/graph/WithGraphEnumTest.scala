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

class WithGraphEnumTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {

  import Checks._
  import Util._
  import Graph._
  
  import GraphEnum._
  
  type Input = (Int, Graph)
  type Output = Graph
  type EnumType = Depend[Input, Output]

  test("enumeration, with graph at root") {
    val checkerHelper = new CheckerHelper[Output]
    import checkerHelper._

    val enum = constructEnumeratorWithGraphAtRoot
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
      res.toList should contain (
        // watch out - order of nodes
//        (List(1), 2, List(1)) &: (Nil, 1, Nil) &: Empty
        (Nil, 1, Nil) &: (List(1), 2, List(1)) &: Empty
      )
    }

  }
  
  test("enumeration, with graph") {
    val checkerHelper = new CheckerHelper[Output]
    import checkerHelper._

    val enum = new WithGraphEnum
    import enum._
    var inputGraph: Graph = null

    withLazyClue(s"Input graph is $inputGraph and elements are $clue") {
      
      res = enumToMax(0, 0)
      res.size should be (1)
      res(0) should be (Empty)
      
      res = enumToMax(1, 0)
      res.size should be (1)
      res(0) should be ((Nil, 1, Nil) &: Empty)

      res = enumToMax(0, 1)
      res.size should be (1)
      res(0) should be (Empty)
      
      res = enumToMax(1, 1)
      res.size should be (4)
      res.toList should contain ((Nil, 2, 1 :: Nil) &: Empty)

      // does not work for Empty
      inputGraph = (Nil, 1, Nil) &: Empty

      res = withSubgraph(0, inputGraph)
      res.size should be (0)

      res = withSubgraph(1, inputGraph)
      res.size should be (1)
      res(0) should be(inputGraph)

      res = withSubgraph(2, inputGraph)
      res.size should be (8)
      res.toList should contain (
        // watch out - order of nodes
//        (List(1), 2, List(1)) &: (Nil, 1, Nil) &: Empty
        (Nil, 1, Nil) &: (List(1), 2, List(1)) &: Empty
      )

      inputGraph = (Nil, 1, Nil) &: (List(1), 2, Nil) &: Empty
     
      res = withSubgraph(0, inputGraph)
      res.size should be (0)

      res = withSubgraph(1, inputGraph)
      res.size should be (0)

      res = withSubgraph(2, inputGraph)
      res.size should be (4)
      res.toList should contain allOf (
        (Nil, 1, Nil) &: (List(1), 2, List(1)) &: Empty,
        (Nil, 1, Nil) &: (List(1), 2, Nil) &: Empty
      )
      
      res = withSubgraph(3, inputGraph)
      res.size should be (128)
      res.toList should contain allOf (
        (Nil, 1, Nil) &: (List(1), 2, Nil) &: (List(2), 3, List(1)) &: Empty,
        (Nil, 1, Nil) &: (Nil, 2, Nil) &: (List(2), 3, Nil) &: Empty
      )
      res.toList should contain noneOf (
        (Nil, 1, Nil) &: (Nil, 2, Nil) &: (Nil, 3, List(1)) &: Empty,
        (Nil, 1, Nil) &: (Nil, 2, Nil) &: (Nil, 3, Nil) &: Empty
      )
    
      inputGraph = (Nil, 1, Nil) &: (List(1), 2, Nil) &: (2 :: Nil, 3, 1 :: Nil) &: Empty
      res = withSubgraph(4, inputGraph)
      res.toList should contain allOf (
        (Nil, 1, Nil) &: (List(1), 2, Nil) &: (List(2), 3, List(1)) &: (2 :: Nil, 4, 1 :: Nil) &: Empty,
        (Nil, 1, Nil) &: (Nil, 2, 1::Nil) &: (2::Nil, 3, Nil) &: (3::Nil, 4, 2::Nil) &: Empty
      )
      res.toList should contain noneOf (
        (Nil, 1, Nil) &: (Nil, 2, Nil) &: (Nil, 3, List(1)) &: (3 :: Nil, 4, 2 :: Nil) &: Empty,
        (Nil, 1, Nil) &: (Nil, 2, Nil) &: (Nil, 3, Nil) &: (Nil, 3, List(1)) &: Empty
      ) 
    }

  }
  
  def constructEnumeratorWithGraphAtRoot(implicit ms: MemoizationScope = null) = {

    import Enum._
    
    val graphs = GraphEnum.graphEnum
    val wrapSubgraph = wrapSubgraphEnum 

    Depend.memoizedFin[Input, Output](
      (self: DependFinite[Input, Output], pair: Input) => {
        val (size, graph) = pair
        
        def wrapSubgraphAtRoot(implicit ms: MemoizationScope = null): OverrideFunctionFin[Graph, Output] = {
          new OverrideFunctionFin[Graph, Output](
            (sup: Graph => Finite[Output], currGraph: Graph) => {
              if (currGraph == Graph.empty) 
                // generate i-th to size layer of general graph, then decrement 
                graphEnumFromToMax(graph.size, size - graph.size)
              else sup(currGraph)
            },
            wrapSubgraph)
        }
        
        val wrapSubgraphRes = wrapSubgraphAtRoot(ms)
        
        wrapSubgraphRes(graph)
      })

  }

  class WithGraphEnum(implicit ms: MemoizationScope = null) {

    import Enum._
    
    val graphs = GraphEnum.graphEnum
    val wrapSubgraph = wrapSubgraphEnum 
    
    val enumToMax = Depend.memoizedFin( (self:DependFinite[(Int, Int), Graph], pair: (Int, Int)) => {
      val (size, existing) = pair
      
      if (size == 0) Singleton(Empty)
      else {
        Map.memoized(
          graphOfOrdinal(existing + 1) product self(size - 1, existing + 1),
            { (in: (Context, Graph)) =>
                val (ctx, restGraph) = in
                ctx &: restGraph
            }): Finite[Graph]
      }
    })
     
    val withSubgraph =
      Depend.memoizedFin[Input, Output](
        (self: DependFinite[Input, Output], pair: Input) => {
          val (size, subgraph) = pair
          
          def subgraphAtLevel(level: Int) = {
            
            val fromToEnum =
              new OverrideFunctionFin[(Int, Int), Graph](
                (sup: ((Int, Int)) => Finite[Graph], input: (Int, Int)) => {
                  val (size, existing) = input
                  
                  if (existing >= level && existing < level + subgraph.size) {
                    def shift(l: List[Int]) = l map { _ + level }
                    val (leftS, _, rightS) = subgraph.contexts(existing - level)
                    
                    sup(input) map {
                      case ((left, ord, right) &: rest) =>
                        ( shift(leftS) ++ left, ord, shift(rightS) ++ right ) &: rest
                    }
                  }
                  else
                    sup(input)
                },
                enumToMax)
                
            fromToEnum(size, 0)
          }
          
          if (size < subgraph.size) scife.enumeration.Empty
          else {
            (Enum(0 to size - subgraph.size) schain
              Depend.memoizedFin( subgraphAtLevel (_: Int) ))
          }
        })
    
//    Depend.memoizedFin[Input, Output](
//      (self: DependFinite[Input, Output], pair: Input) => {
//        val (size, graph) = pair
//        
//        if (size < graph.size) scife.enumeration.Empty
//        else {
//          constructEnumeratorWithGraphAtRoot(ms)( size, graph ) concat
//            (graphOfOrdinal(size) product self(size-1, graph) map {
//              (in: (Context, Graph)) =>
//                val (curr, rest) = in
//                curr &: rest
//            })          
//        }
//      })

  }

}
