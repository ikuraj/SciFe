package insynth.util

// useful for testing generation of RB trees and then invoking operations on them
object RedBlackTreeWithOperations {
  import scala.language.implicitConversions
  
  import structures.{ RedBlackTrees => srb }
    
  // conversion from helper class rbtrees to these ones
  implicit def thisRBTreeToStandardRBTree(tree: Tree): srb.Tree =
    tree match {
      case Leaf => srb.Leaf
      case Node(c, l, v, r) =>
        srb.Node(thisRBTreeToStandardRBTree(l), v, thisRBTreeToStandardRBTree(r), c)
    }

  abstract class Tree {

    type V = Int

    // blacken: Turn a node black.
    protected def blacken(n: Tree): Tree = {
      n match {
        case Leaf => n
        case Node(_, l, v, r) => Node(true, l, v, r)
      }
    }

    // balance: Balance a tree with balanced subtrees.
    protected def balance(c: Boolean)(l: Tree)(v: Int)(r: Tree): Tree = {
      (c, l, v, r) match {
        case (true, Node(false, Node(false, a, xV, b), yV, c), zV, d) =>
          Node(false, Node(true, a, xV, b), yV, Node(true, c, zV, d))
        case (true, Node(false, a, xV, Node(false, b, yV, c)), zV, d) =>
          Node(false, Node(true, a, xV, b), yV, Node(true, c, zV, d))
        case (true, a, xV, Node(false, Node(false, b, yV, c), zV, d)) =>
          Node(false, Node(true, a, xV, b), yV, Node(true, c, zV, d))
        case (true, a, xV, Node(false, b, yV, Node(false, c, zV, d))) =>
          Node(false, Node(true, a, xV, b), yV, Node(true, c, zV, d))
        case (c, a, xV, b) => Node(c, a, xV, b)
      }
    }
    
    protected def balance(t: Tree): Tree = t match {
      case Leaf => Leaf
      case Node(c, l, v, r) => balance(c)(l)(v)(r)
    }

    // insert: Insert a value at a key.
    def insert(v: V) = blacken(insertWith(v))
    
    def insertWith(v: V): Tree
    
    // get: Retrieve a value for a key.
    def contains(k : V): Boolean
    
    def hasSubtree(t: Tree): Boolean

  }

  // A leaf node.
  case object Leaf extends Tree {

    override def insertWith(v: V) = Node(false, Leaf, v, Leaf)
    
    def contains(k : V) = false

    def hasSubtree(t: Tree) = t == Leaf

  }

  // A tree node.
  case class Node(val c: Boolean, val l: Tree, val v: Int, val r: Tree) extends Tree {

    override def insertWith(v: V) = {
      if (v < this.v) balance(c)(l.insertWith(v))(this.v)(r)
      else if (v == this.v) this//Node(c, l, v, r)
      else balance(c)(l)(this.v)(r.insertWith(v))
    }
    
    def contains(v : Int) = {
      if (v < this.v) l.contains(v)
      else if (v > this.v) r.contains(v)
      else true
    }

    def hasSubtree(t: Tree) = t match {
      case Leaf => true
      case n: Node =>
        if (n.v < this.v) l.hasSubtree(t)
        else if (n.v > this.v) r.hasSubtree(t)
        else t == this
    }
    
//    override def toString = "Node(%b, %s, %d, %s)".format(c, l, v, r)

  }
  
//  object Node {
//    def apply(c: Boolean, l: Tree, v: Int, r: Tree) =
//      new Node(c, l, v, r)
//    
//    def unapply(n: Node) =
//      Some( (n.c, n.l, n.v, n.r) )
//  }
}