package scife.enumeration
package testcases
package graph

object LabeledGraph {
  type Node = Int
  type Adj[+B] = List[(B, Node)]
  type Context[+A, +B] = (Adj[B], Node, A, Adj[B])

  object Graph {
    def empty[A, B]: Graph[A, B] =
      Empty: Graph[A, B]
  }
  
  def &:[A, B](context: Context[A, B], graph: Graph[A, B]): Graph[A, B] =
    new &:(context, graph)

  sealed trait Graph[+A, +B] {
    def &:[AA >: A, BB >: B](context: Context[AA, BB]): Graph[AA, BB] =
      new &:(context, this)

    /** Abstract methods start */
    def isEmpty: Boolean

    def ufold[C](z: C)(op: (Context[A, B], C) => C): C
    /** Abstract methods end */

    /** TODO */
    def contextMatch(v: Node): (Option[Context[A, B]], Graph[A, B]) = ???

    def deg(v: Node): Int =
      contextMatch(v) match {
        case (Some((p, _, _, s)), _) => p.size + s.size
        case _ => throw new Exception(s"Node $v not found in graph.")
      }

    def del(v: Node): Graph[A, B] =
      contextMatch(v) match {
        case (Some(_), g) => g
        case _ => throw new Exception(s"Node $v not found in graph.")
      }

    def gmap[C, D](func: Context[A, B] => Context[C, D]): Graph[C, D] =
      ufold(Graph.empty[C, D])((context, acc) => func(context) &: acc)

    def grev: Graph[A, B] =
      gmap {
        case (p, v, l, s) => (s, v, l, p)
      }

    def gsuc(v: Node): List[Node] =
      contextMatch(v) match {
        case (Some((_, _, _, s)), g) => s.map(_._2)
        case _ => throw new Exception(s"Node $v not found in graph.")
      }

    def nodes: List[Node] =
      ufold(List.empty[Node]) {
        case ((_, v, _, _), acc) => v :: acc
      }

    def undir: Graph[A, B] =
      gmap {
        case (p, v, l, s) =>
          val ps = (p ++ s).distinct
          (ps, v, l, ps)
      }
  }

  case object Empty extends Graph[Nothing, Nothing] {
    override def isEmpty: Boolean =
      true

    override def ufold[C](z: C)(op: (Context[Nothing, Nothing], C) => C): C =
      z
  }

  case class &:[+A, +B](context: Context[A, B], graph: Graph[A, B]) extends Graph[A, B] {
    override def isEmpty: Boolean =
      false

    override def ufold[C](z: C)(op: (Context[A, B], C) => C): C =
      op(context, graph.ufold(z)(op))
  }

  def main(args: Array[String]) {
    val graph = (List(("left", 2), ("up", 3)), 1, 'a', List(("right", 2))) &:
      (Nil, 2, 'b', List(("down", 3))) &:
      (Nil, 3, 'c', Nil) &:
      Empty

    println(graph)

    println("=================================")

    println(graph.nodes)

    println("=================================")

    println(graph.undir)

    println("=================================")

    println(graph.gsuc(2))
  }
}
