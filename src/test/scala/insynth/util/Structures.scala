package insynth.util

import insynth.attrgrammar._
import insynth.reconstruction.stream._

import org.scalatest._
import org.scalatest.matchers._
import org.scalatest.prop.Checkers

import org.scalacheck._
import Gen._

import scala.language.implicitConversions
import scala.language.postfixOps

object Structures {
  
  import StreamableAST._
  
  val fromOne = Stream.from(1)

  def generateLists(maxSize: Int, integers: Range) = {
    def rec(sizeToGen: Int): List[List[Int]] = {
      if (sizeToGen == 0) List(Nil)
      else {
        val result =
          for ( el <- integers; 
            recList <- rec(sizeToGen - 1))
            yield el :: recList
          
        result.toList
      }
    }
    
    rec(maxSize)
  }
  
  case class JustInt(a: Int)
  trait CusList
  case object CusNil extends CusList
  case class Cons(e: JustInt, list: CusList) extends CusList
  
  object CusList {
    def isSorted(list: CusList) = {
      def rec(l: CusList): Boolean = l match {
        case Cons(JustInt(a), innerList@Cons(JustInt(b), _)) =>
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

  object RedBlackTrees {    
    trait Tree
    case object Leaf extends Tree
    // if c == true then the node is black
    case class Node(l: Tree, v: Int, r: Tree, c: Boolean) extends Tree
    
    def invariant(tree: Tree) =
      blackInv(tree) && redDescHaveBlackChildren(tree) && valueOrdering(tree)
        
    // every path from the root to a leaf has the same number of black nodes
    def blackInv(tree: Tree) = {
      def rec(t : Tree) : (Boolean, Int) = t match {
        case Node(l, _, r, c) =>
          val (lRes, lHeight) = rec(l)
          val (rRes, rHeight) = rec(r)
          val newHeight =
            if (c) lHeight + 1
            else lHeight
          
          ((lRes && rRes && lHeight == rHeight), lHeight)
        case Leaf => (true, 1)
      }
      
      rec(tree)._1
    }
  
    // no red node has a red child
    def redDescHaveBlackChildren(t: Tree) : Boolean = {    
      def isBlack(t: Tree) : Boolean = t match {
        case Leaf => true
        case Node(_,_,_, color) => color
      }
      def redNodesHaveBlackChildren(t: Tree) : Boolean = t match {
        case Leaf => true
        case Node(l, _, r, true) => redNodesHaveBlackChildren(l) && redNodesHaveBlackChildren(r)
        case Node(l, _, r, false) => isBlack(l) && isBlack(r)
          redNodesHaveBlackChildren(l) && redNodesHaveBlackChildren(r)
        case _ => false
      }
      t match {
        case Leaf => true
        case Node(l,_,r, c) => redNodesHaveBlackChildren(l) && redNodesHaveBlackChildren(r)
      }
    }
    
		// for every node n, all the nodes in the left (respectively, right) subtree of
		// n, if any, have keys which are smaller (respectively, bigger) than the key
  	// labeling n.
    def valueOrdering(t: Tree) : Boolean = {
      def valuesInRange(t: Tree, min: Int, max: Int): Boolean = t match {
        case Leaf => true
        case Node(l, v, r, c) => min <= v && max > v &&
        	valuesInRange(l, min, v) && valuesInRange(r, v + 1, max)
      }
      
      valuesInRange(t, Int.MinValue, Int.MaxValue)
    }

    // conversion from helper class rbtrees to these ones
    def rbMap2rbTree[V](rbMap: RBMap[Int, V]): Tree = rbMap match {
      case leaf: L[Int, V] => Leaf
      case T(c, l, k, v, r) =>
        val color = c == RBTreeItems.B
        Node(rbMap2rbTree(l), k, rbMap2rbTree(r), color)
    }
  }

  object Binomial {
    def main(args: Array[String]): Unit = {
      val n = 5
      val k = 3
      val result = binomialCoefficient(n, k)
      println("The Binomial Coefficient of %d and %d equals %d.".format(n, k, result))
    }

    def binomialCoefficient(n: Int, k: Int): Int = 
      ( (BigInt(n - k + 1) to n).product / 
      (BigInt(1) to k).product).intValue
  }
  
}

class StructuresTest extends FunSuite with ShouldMatchers {
  
  import Structures._
  
  test("generate lists") {
    {
      val lists = generateLists(3, 1 to 3)
      
      lists.size should be (3 * 3 * 3)
    }
    
    {
      val lists = generateLists(1, 1 to 1)
      
      lists.size should be (1)
    }    
    
    {
      val lists = generateLists(2, 1 to 1)
      
      lists.size should be (1)
    }    
  }
  
  import CusList._ 
  
  test("isSorted") {
    
    for(ex <- List(
      List(1, 2, 2),
      List(1, 2),
      List(1, 1),
      List(1, 1, 2, 2, 3),
      List(1, 1, 1, 1, 1, 2, 3)
    ): List[CusList])
      isSorted(ex) should be (true)
    
    for(ex <- List(
      List(1, 3, 2),
      List(3, 2),
      List(3, 1),
      List(1, 1, 2, 2, 1),
      List(1, 1, 1, 1, 1, 2, 1)
    ): List[CusList])
      isSorted(ex) should be (false)
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
      
    Prop.forAll(rbTreeGen)(invariant) check
  }
}