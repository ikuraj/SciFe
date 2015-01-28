package scife.enumeration
package testcases
package graph

object Graph {
  type Node = Int
  type Adj = List[Node]
  type Context = (Adj, Node, Adj)

  object Graph {
    def empty: Graph =
      Empty: Graph
  }
  
  def &:(context: Context, graph: Graph): Graph =
    new &:(context, graph)

  sealed trait Graph {
    def &:(context: Context): Graph =
      new &:(context, this)

    /** Abstract methods start */
    def isEmpty: Boolean

    def ufold[C](z: C)(op: (Context, C) => C): C
    
    def size: Int
    /** Abstract methods end */

    /** TODO */
    def contextMatch(v: Node): (Option[Context], Graph) = ???

    def gmap[C, D](func: Context => Context): Graph =
      ufold(Graph.empty)((context, acc) => func(context) &: acc)

    def grev: Graph =
      gmap {
        case (p, v, s) => (s, v, p)
      }

    def nodes: List[Node] =
      ufold(List.empty[Node]) {
        case ((_, v, _), acc) => v :: acc
      }

    def undir: Graph =
      gmap {
        case (p, v, s) =>
          val ps = (p ++ s).distinct
          (ps, v, ps)
      }
  }

  case object Empty extends Graph {
    override def isEmpty: Boolean =
      true
      
    override def size = 0

    override def ufold[C](z: C)(op: (Context, C) => C): C =
      z
  }

  case class &:(context: Context, graph: Graph) extends Graph {
    override def isEmpty: Boolean =
      false
      
    def size = this.ufold(0) {
      case (_, acc) => 1 + acc
    }

    override def ufold[C](z: C)(op: (Context, C) => C): C =
      op(context, graph.ufold(z)(op))
  }

  def main(args: Array[String]) {
    val graph = (List(2, 3), 1, List(2)) &:
      (Nil, 2, List(3)) &:
      (Nil, 3, Nil) &:
      Empty

    println(graph)

    println("=================================")

    println(graph.nodes)

    println("=================================")

    println(graph.undir)

    println("=================================")

  }
}
