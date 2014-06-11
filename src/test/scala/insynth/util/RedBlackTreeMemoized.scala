//package insynth.util
//
//object RedBlackTreeWithOperationsMemoized {
//  import RedBlackTreeWithOperations._
//  import Structures.{ RedBlackTrees => srb }
//
//  import scala.language.implicitConversions
//    
//  abstract class Tree extends RedBlackTreeWithOperations.Tree {
//    
//    override protected def blacken(n: RedBlackTreeWithOperations.Tree):
//      RedBlackTreeWithOperations.Tree = {
//      n match {
//        case Leaf => n
//        case Node(false, l, v, r, s, bh) => Node(true, l, v, r, s, bh + 1)
//        case Node(true, l, v, r, s, bh) => Node(true, l, v, r, s, bh)
//      }
//    }
//    
//    def size: Int
//    
//    def blackHeight: Int
//
//  }
//
//  // A leaf node.
//  case object Leaf extends Tree {
//
//    override def insertWith(v: V) = Node(false, Leaf, v, Leaf, 1, 0)
//    
//    def contains(k : V) = false
//    
//    def size = 0 
//    
//    def blackHeight = 1
//
//  }
//
//  // A tree node.
//  case class Node(c: Boolean, l: Tree, v: Int, r: Tree, size: Int, blackHeight: Int)
//    extends RedBlackTreeWithOperations.Node(c, l, v, r)
//
//}