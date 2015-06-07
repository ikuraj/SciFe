package scife.util

import Math._

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

import scala.language.implicitConversions
import scala.language.postfixOps

package structures {

  case class JustInt(a: Int)
  trait CusList
  case object CusNil extends CusList
  case class Cons(e: JustInt, list: CusList) extends CusList

  object CusList {
    def isSorted(list: CusList) = {
      def rec(l: CusList): Boolean = l match {
        case Cons(JustInt(a), innerList @ Cons(JustInt(b), _)) =>
          a <= b && rec(innerList)
        case _ => true
      }

      rec(list)
    }

    def size(list: CusList) = {
      def rec(l: CusList): Int = l match {
        case CusNil => 0
        case Cons(el, inList) => 1 + rec(inList)
      }

      rec(list)
    }

    implicit def toCusList(list: List[Int]) = {
      def rec(l: List[Int]): CusList = l match {
        case Nil => CusNil
        case el :: inList =>
          Cons(JustInt(el), rec(inList))
      }

      rec(list)
    }
  }

  object TreeShapes {
    trait Tree
    case object Leaf extends Tree
    case class Node(l: Tree, r: Tree) extends Tree

    def size(tree: Tree) = {
      def rec(t: Tree): Int = t match {
        case Leaf => 1
        case Node(l, r) => 1 + rec(l) + rec(r)
      }

      rec(tree)
    }

    def sizeJustNodes(tree: Tree) = {
      def rec(t: Tree): Int = t match {
        case Leaf => 0
        case Node(l, r) => 1 + rec(l) + rec(r)
      }

      rec(tree)
    }
  }

  object BSTrees {
    trait Tree
    case object Leaf extends Tree
    case class Node(l: Tree, v: Int, r: Tree) extends Tree {
      def this(v: Int) = this(Leaf, v, Leaf)
    }

    object Node {
      def apply(v: Int) = new Node(v)
    }

    def invariant(tree: Tree) =
      valueOrdering(tree)

    def valuesInRange(t: Tree, min: Int, max: Int): Boolean = t match {
      case Leaf => true
      case Node(l, v, r) => min <= v && max >= v &&
        valuesInRange(l, min, max) && valuesInRange(r, min, max)
    }

    // for every node n, all the nodes in the left (respectively, right) subtree of
    // n, if any, have keys which are smaller (respectively, bigger) than the key
    // labeling n.
    def valueOrdering(t: Tree): Boolean = {
      def correctOrdering(t: Tree, min: Int, max: Int): Boolean = t match {
        case Leaf => true
        case Node(l, v, r) => min <= v && max > v &&
          correctOrdering(l, min, v) && correctOrdering(r, v + 1, max)
      }

      correctOrdering(t, Int.MinValue, Int.MaxValue)
    }

    def size(t: Tree): Int = t match {
      case Leaf => 0
      case Node(l, v, r) => 1 + size(l) + size(r)
    }

    def numberOfTress(n: Int) = Catalan.catalan(n)

    def heapProperty(t: Tree): Boolean = {
      t match {
        case Node(ln @ Node(_, lv, _), v, rn @ Node(_, rv, _)) => v > lv && v > rv &&
          heapEqualProperty(ln) && heapEqualProperty(rn)
        case Node(_, v, rn @ Node(_, rv, _)) => v > rv &&
          heapEqualProperty(rn)
        case Node(ln @ Node(_, lv, _), v, _) => v > lv &&
          heapEqualProperty(ln)
        case _ => true
      }
    }

    def heapEqualProperty(t: Tree): Boolean = {
      t match {
        case Node(ln @ Node(_, lv, _), v, rn @ Node(_, rv, _)) => v >= lv && v >= rv &&
          heapEqualProperty(ln) && heapEqualProperty(rn)
        case Node(_, v, rn @ Node(_, rv, _)) => v >= rv &&
          heapEqualProperty(rn)
        case Node(ln @ Node(_, lv, _), v, _) => v >= lv &&
          heapEqualProperty(ln)
        case _ => true
      }
    }

  }

  object LazyBSTrees {
    trait Tree {
      def correctOrdering(min: Int, max: Int): Boolean
      def lazyInvariant: Boolean =
        correctOrdering(Int.MinValue, Int.MaxValue)
    }
    case object Leaf extends Tree {
      def correctOrdering(min: Int, max: Int): Boolean = false
    }
    //    class Node(_l: => Tree, _v: => Int, _r: => Tree) extends Tree {
    //      lazy val l = _l
    //      lazy val v = _v
    //      lazy val r = _r
    //      
    //      override def equals(that: Any) = that match {
    //        case thatNode: Node =>
    //          thatNode.v == v && thatNode.l == l && thatNode.r == r
    //        case _ => false
    //      }
    //    }
    class Node(_l: => Tree, val v: Int, _r: => Tree) extends Tree {
      private[this] var lf: Tree = null
      private[this] var rf: Tree = null

      def l = {
        if (lf == null) lf = _l
        lf
      }

      def r = {
        if (rf == null) rf = _r
        rf
      }

      override def equals(that: Any) = that match {
        case thatNode: Node =>
          thatNode.v == v && thatNode.l == l && thatNode.r == r
        case _ => false
      }

      def correctOrdering(min: Int, max: Int): Boolean = {
        min <= v && max > v &&
          (lf == null || lf.correctOrdering(min, v)) &&
          (rf == null || rf.correctOrdering(v + 1, max))
      }
      
      override def toString =
        "Node(" + l.toString + "," + v + "," + r.toString + ")"
    }

    object Node {
      def apply(l: => Tree, v: => Int, r: => Tree) = new Node(l, v, r)
      def apply(v: => Int) = new Node(Leaf, v, Leaf)
    }

    implicit def toRegularBSTTree(t: Tree): BSTrees.Tree = t match {
      case Leaf => BSTrees.Leaf
      case n: Node => BSTrees.Node(
        toRegularBSTTree(n.l), n.v, toRegularBSTTree(n.r))
    }

    def insert(tree: Tree, newElem: Int): Tree = tree match {
      case Leaf =>
        Node(Leaf, newElem, Leaf)
      case n: Node =>
        if (newElem < n.v) insert(n.l, newElem)
        else if (newElem > n.v) insert(n.r, newElem)
        else tree
    }
  }

  object RedBlackTrees {
    trait Tree
    case object Leaf extends Tree
    // if c == true then the node is black
    case class Node(l: Tree, v: Int, r: Tree, c: Boolean) extends Tree

    def invariant(tree: Tree) =
      blackInv(tree) && redDescHaveBlackChildren(tree) && valueOrdering(tree)

    def blackHeight(t: Tree): Int = t match {
      case Node(l, _, r, c) =>
        val lHeight = blackHeight(l)
        val newHeight =
          if (c) lHeight + 1
          else lHeight

        newHeight
      case Leaf => 1
    }

    // every path from the root to a leaf has the same number of black nodes
    def blackInv(tree: Tree) = {
      def rec(t: Tree): (Boolean, Int) = t match {
        case Node(l, _, r, c) =>
          val (lRes, lHeight) = rec(l)
          val (rRes, rHeight) = rec(r)
          val newHeight =
            if (c) lHeight + 1
            else lHeight

          ((lRes && rRes && lHeight == rHeight), newHeight)
        case Leaf => (true, 1)
      }

      rec(tree)._1
    }

    // no red node has a red child
    def redDescHaveBlackChildren(t: Tree): Boolean = {
      def isBlack(t: Tree): Boolean = t match {
        case Leaf => true
        case Node(_, _, _, color) => color
      }
      def redNodesHaveBlackChildren(t: Tree): Boolean = t match {
        case Leaf => true
        case Node(l, _, r, true) => redNodesHaveBlackChildren(l) && redNodesHaveBlackChildren(r)
        case Node(l, _, r, false) => isBlack(l) && isBlack(r) &&
          redNodesHaveBlackChildren(l) && redNodesHaveBlackChildren(r)
      }
      redNodesHaveBlackChildren(t)
    }

    // for every node n, all the nodes in the left (respectively, right) subtree of
    // n, if any, have keys which are smaller (respectively, bigger) than the key
    // labeling n.
    def valueOrdering(t: Tree): Boolean = {
      def valuesInRange(t: Tree, min: Int, max: Int): Boolean = t match {
        case Leaf => true
        case Node(l, v, r, c) => min <= v && max > v &&
          valuesInRange(l, min, v) && valuesInRange(r, v + 1, max)
      }

      valuesInRange(t, Int.MinValue, Int.MaxValue)
    }

    def size(t: Tree): Int = t match {
      case Leaf => 0
      case Node(l, v, r, c) => 1 + size(l) + size(r)
    }

    // conversion from helper class rbtrees to these ones
    def rbMap2rbTree[V](rbMap: RBMap[Int, V]): Tree = rbMap match {
      case leaf: L[Int, V] => Leaf
      case T(c, l, k, v, r) =>
        val color = c == RBTreeItems.B
        Node(rbMap2rbTree(l), k, rbMap2rbTree(r), color)
    }

    def numberOfTrees =
      Stream.from(0) zip List(
        1, 2, 2, 3, 8, 14, 20, 35, 64, 122, 260, 586, 1296, 2708, 5400,
        10468, 19888, 37580, 71960, 140612, 279264, 560544, 1133760, 2310316,
        4750368, 9876264, 20788880, 44282696, 95241664, 206150208, 447470464,
        970862029, 2100029344) toMap

    // helper method when constructing streams
    val constructRBTree: PartialFunction[Any, Tree] = (a: Any) => a match {
      case (clazz, (a: Tree) :: (v: Int) :: (b: Tree) :: (c: Boolean) :: Nil) if clazz == classOf[Node] =>
        Node(a, v, b, c)
    }

    def blackHeightRange(size: Int) = Math.log2(size).toInt to (Math.log2(size + 1).toInt + 1)

    def rangeOfValues(t: Tree): Set[Int] = t match {
      case Leaf => Set()
      case Node(l, v, r, _) =>
        rangeOfValues(l) + v union rangeOfValues(r)
    }

    def rootColor(t: Tree) = t match {
      // leaves are black
      case Leaf => true
      case Node(_, _, _, c) => c
    }

  }

  object RedBlackTreesFastHash {
    trait Tree
    case object Leaf extends Tree
    // if c == true then the node is black
    case class Node(l: Tree, v: Int, r: Tree, c: Boolean) extends Tree {
      override def hashCode =
        super.hashCode()
    }
  }

  object Program {
    implicit def intToId(i: Int) = Id(i)
    case class Id(id: Int)

    trait Identifiable

    case class Program(classes: Seq[Class])
    case class Class(id: Id, methods: Seq[Method]) extends Identifiable
    case class Method(id: Id, statements: Seq[Statement]) extends Identifiable {
      require(
        (for (ind <- 0 until statements.size)
          yield usedVars(statements(ind)) subsetOf definedVars(statements.take(ind))).reduce(_ && _))
      def usedVars(s: Statement) = Set[Id]()
      def definedVars(s: Seq[Statement]) = Set[Id]()
    }
    abstract class Statement
    case class MethodCall(callee: Id, argument: Seq[Expression])
    case class Assignment(varId: Id, exp: Expression) extends Statement with Identifiable
    abstract class Expression
    case class Var(id: Id) extends Expression
    case class IntExp(v: Int) extends Expression
    case class BooleanExp(v: Boolean) extends Expression

    object BinOp extends Enumeration {
      val Plus, Minus = Value
    }
    case class BinOp(l: Expression, r: Expression) extends Expression
    case class UnOp(l: Expression, r: Expression) extends Expression
  }

}

class StructuresTest extends FunSuite with Matchers with PropertyChecks {

  import Common._
  import structures._

  test("generate lists") {
    {
      val lists = generateLists(3, 1 to 3)

      lists.size should be(3 * 3 * 3)
    }

    {
      val lists = generateLists(1, 1 to 1)

      lists.size should be(1)
    }

    {
      val lists = generateLists(2, 1 to 1)

      lists.size should be(1)
    }
  }

  import CusList._

  test("isSorted") {

    for (
      ex <- List(
        List(1, 2, 2),
        List(1, 2),
        List(1, 1),
        List(1, 1, 2, 2, 3),
        List(1, 1, 1, 1, 1, 2, 3)): List[CusList]
    ) isSorted(ex) should be(true)

    for (
      ex <- List(
        List(1, 3, 2),
        List(3, 2),
        List(3, 1),
        List(1, 1, 2, 2, 1),
        List(1, 1, 1, 1, 1, 2, 1)): List[CusList]
    ) isSorted(ex) should be(false)
  }

  test("generate RB trees") {
    import RedBlackTrees._

    val rbTreeGen =
      for {
        size <- Gen.choose(1, 10)
        values <- Gen.listOfN(size, Gen.choose(10, 50))
      } yield {
        val rbMap = RBMap(values map (x => (x, null)): _*)

        rbMap2rbTree(rbMap)
      }

    forAll(rbTreeGen, minSuccessful(50)) {
      invariant(_) should be(true)
    }
  }

  test("catalan numbers") {
    import Catalan._

    val resultList = List(
      1,
      1,
      2,
      5,
      14,
      42,
      132,
      429,
      1430,
      4862,
      16796,
      58786,
      208012,
      742900,
      2674440)

    for (ind <- 0 until resultList.size)
      catalan(ind) should be(resultList(ind))
  }
}
