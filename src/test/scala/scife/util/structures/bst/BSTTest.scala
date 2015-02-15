package scife.util.structures.bst

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

import ybanez._

class BSTTest extends FunSuite with Matchers {

  test("main operations") {
    val bst = BST() + 6 + 4 + 2 + 1 + 3 ++ BST(5, 8, 7, 10, 9)

    assert(bst.contains(5), "contains 5")
    assert(!bst.contains(11), "contains 11")
    assert(!bst.exists(_ == 11))
    assert(bst.exists(_ == 5))
    assert(!bst.exists(_ == 11))
    assert(bst.filter(_ % 2 == 0).toList == List(6, 4, 2, 8, 10))
//    println("contains 11: " + bst.contains(11))
//    println("exists 5: " + bst.exists(_ == 5))
//    println("exists 11: " + bst.exists(_ == 11))
//    println("toString: " + bst)
//    println("filter 2: " + bst.filter(_ % 2 == 0).toList)
//    println("map: " + bst.map(_ + 1).toList)
//    println("flatMap: " + bst.flatMap(x => BST(x + 1)).toList)
//    println()
//    println("Traversals...")
//    printTraversals(bst)
    bst.size should be (10)
    EmptyBST.size should be (0)
    
    val (opt, bst1) = bst - 6
    opt should contain (6)
    bst1.toList should not contain (6)
//    println()
//    println("Remove 6: " + opt.getOrElse("Not found"))
//    println("Rest of bst...")
//    printTraversals(bst1)
    
    // to check if it's a monad    
    def f(x: Int) = BST(x + 1)
    def g(x: Int) = BST(x + 2)
//    println()
//    println("map and flatMap: " + (bst.map(_ + 1) == bst.flatMap(x => BST(x + 1))))
    bst.map(_ + 1) should be (bst.flatMap(x => BST(x + 1)))
//    println("Associativity: " + (bst.flatMap(f).flatMap(g) == bst.flatMap(x => f(x).flatMap(g))))
    bst.flatMap(f).flatMap(g) should be (bst.flatMap(x => f(x).flatMap(g)))
//    println("Left Unit: " + (BST(1).flatMap(f) == f(1)))
    BST(1).flatMap(f) should be (f(1))
//    println("Right Unit: " + (bst.flatMap(BST(_)) == bst))
    bst.flatMap(BST(_)) should be(bst)
  }
  
  def printTraversals[T](bst: BST[T]) = {
    println("preOrder: " + bst.preOrder(List[T]())(_ :: _).reverse)
    println("postOrder: " + bst.postOrder(List[T]())(_ :: _).reverse)
    println("inOrder: " + bst.inOrder(List[T]())(_ :: _).reverse)
    println("levelOrder: " + bst.levelOrder(List[T]())(_ :: _).reverse)
  }
}