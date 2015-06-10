package scife
package enumeration
package showcase
package oopsla15

import dependent._
import memoization._
import scife.{ enumeration => e }

import scalaz.LazyTuple2

import scife.util._

import org.scalatest._
import org.scalatest.prop._
import org.scalameter.api._

import scala.language.postfixOps

// import DSL
import Enum._
import Depend._
import lazytraversal._

// import the lazy data structure
import common._
import enumdef._
import scife.util.structures._
import LazyBSTrees._

class BinarySearchTreeLazyTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {

  test("Binary search tree, lazy enumeration") {
    import BinarySearchTreeLazyTest._

    val bst =
      new WrapFunctionTest[(Int, Range), Tree, EnumType](
        (self: DepEnumType[(Int, Range), Tree], pair: (Int, Range)) => {
          val (size, range) = pair

          if (size <= 0) new e.Singleton((Leaf: Tree)) with Touchable[Tree] with Resetable[Tree] with NoSkip[Tree]
          else {
            val leftTrees: DepEnumTypeFinite[(Int, Int), Tree] = new InMap(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (leftSize, range.start to (median - 1))
            }) with DependFinite[(Int, Int), Tree] {
              override type EnumSort[A] = BinarySearchTreeLazyTest.EnumType[A]
            }

            val rightTrees: DepEnumTypeFinite[(Int, Int), Tree] = new InMap(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (size - leftSize - 1, (median + 1) to range.end)
            }) with DependFinite[(Int, Int), Tree] {
              override type EnumSort[A] = BinarySearchTreeLazyTest.EnumType[A]
            }

            new lazytraversal.ChainFiniteSingleCombine(
              (0 until size) ⊗ range,
              lazytraversal.dependent.ProductFinite(leftTrees, rightTrees),
              fConstructTree
            ) with Touchable[Tree]
          }
        })

    val size = 5
    var count = 0

    // test for inserting all elements up to size
    for (x <- 1 to size) {
      val enum = bst(size - 1, 1 to size)
      var nextInd = 0

      while (nextInd < enum.size) {
        // get the next tree
        val t = enum(nextInd)
        val newT = t insert x
        assert(newT.lazyInvariant)

        // get next index potentially skipping elements
        nextInd = enum.next(nextInd)
        count += 1
        // after each operation, reset enumerator tracking mechanism
        enum.reset
      }
    }

    println(s"For trees of size $size, enumerated only $count out of total ${bst(size - 1, 1 to size).size * size} trees.")
  }

}

object BinarySearchTreeLazyTest {
  
  type EnumType[A] = Finite[A] with Touchable[A] with Resetable[A] with Skippable[A]
  type DepEnumType[I, O] = Depend[I, O] { type EnumSort[A] = BinarySearchTreeLazyTest.this.EnumType[A] }
  type DepEnumTypeFinite[I, O] = DependFinite[I, O] { type EnumSort[A] = BinarySearchTreeLazyTest.this.EnumType[A] }
  
  // helper function that constructs trees
  val fConstructTree: ((Int, Int), => LazyTuple2[Tree, Tree]) => Tree =
    (p1, p2) => Node(p2._1, p1._2, p2._2)
  
}