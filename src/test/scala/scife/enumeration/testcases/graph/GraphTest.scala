package scife.enumeration
package testcases
package graph

class GraphTest extends DefaultTest {

  test("Labeled graph and ops") {
    import LabeledGraph._
    
    val graph = (List(("left", 2), ("up", 3)), 1, 'a', List(("right", 2))) &:
      (Nil, 2, 'b', List(("down", 3))) &:
      (Nil, 3, 'c', Nil) &:
      Empty
      
    graph.nodes
    // not implemented
//    println(graph.undir)
//    println(graph.gsuc(2))
  }
  
  test("Simple graph and ops") {
    import Graph._
    
    val graph = (List(1, 2), 3, List(2)) &:
      (List(1), 2, List(1)) &:
      (Nil, 1, Nil) &:
      Empty
      
    graph.size should be (3)
    Empty.size should be (0)
    
    graph.nodes(0) should be (3)
    graph.contexts(0) should be ((List(1, 2), 3, List(2)))

    // not implemented
//    println(graph.undir)
//    println(graph.gsuc(2))
  }

}
