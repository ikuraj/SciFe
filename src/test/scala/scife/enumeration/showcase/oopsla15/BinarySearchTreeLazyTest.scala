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

class BinarySearchTreeLazyTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {

  // import DSL
  import Enum._
  import Depend._
  import lazytraversal._

  import common._
  import enumdef._
  import scife.util.structures._
  import LazyBSTrees._

  type EnumType[A] = Finite[A] with Touchable[A] with Resetable[A] with Skippable[A]
  type DepEnumType[I, O] = Depend[I, O] { type EnumSort[A] = BinarySearchTreeLazyTest.this.EnumType[A] }
  type DepEnumTypeFinite[I, O] = DependFinite[I, O] { type EnumSort[A] = BinarySearchTreeLazyTest.this.EnumType[A] }

  test("Binary search tree, lazy enumeration") {

    val bst =
      new WrapFunctionTest[(Int, Range), Tree, EnumType](
        ((self: DepEnumType[(Int, Range), Tree], pair: (Int, Range)) => {
          val (size, range) = pair

          if (size < 0) throw new RuntimeException
          if (size <= 0) new e.Singleton((Leaf: Tree)) with Touchable[Tree] with Resetable[Tree] with NoSkip[Tree] {
            override def toString = s"Singleton[$hashCode]"
          }
          else if (range.isEmpty) Empty
          else if (size == 1)
            new e.WrapArray(range map { v => (Node(Leaf, v, Leaf): Tree) } toArray) with Touchable[Tree] with Resetable[Tree] with NoSkip[Tree] {

              //            override def toString = s"Array[$hashCode](${toList})"
              override def toString = s"Array[$hashCode]()"
            }
          else {
            val leftSizes = e.Enum(0 until size)
            val roots = e.Enum(range)

            val rootLeftSizePairs = e.Product(leftSizes, roots)

            val leftTrees: DepEnumTypeFinite[(Int, Int), Tree] = new InMap(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (leftSize, range.start to (median - 1))
            }) with DependFinite[(Int, Int), Tree] {
              override type EnumSort[A] = BinarySearchTreeLazyTest.this.EnumType[A]
            }

            val rightTrees: DepEnumTypeFinite[(Int, Int), Tree] =
              new InMap(self, { (par: (Int, Int)) =>
                val (leftSize, median) = par
                (size - leftSize - 1, (median + 1) to range.end)
              }) with DependFinite[(Int, Int), Tree] {
                override type EnumSort[A] = BinarySearchTreeLazyTest.this.EnumType[A]
              }

            val leftRightPairs: DepEnumTypeFinite[(Int, Int), LazyTuple2[Tree, Tree]] =
              lazytraversal.dependent.ProductFinite(leftTrees, rightTrees)

            val fConstructTree: ((Int, Int), => LazyTuple2[Tree, Tree]) => Tree =
              (p1, p2) => {
                //              print(s"map invoked: ${p1}")
                Node(p2._1, p1._2, p2._2)
              }

            val allNodes =
              new lazytraversal.ChainFiniteSingleCombine[(Int, Int), LazyTuple2[Tree, Tree], Tree](
                rootLeftSizePairs, leftRightPairs,
                fConstructTree) with Touchable[Tree] {
                override def toString = s"ChainFiniteSingleCombine[$hashCode]"
              }

            allNodes: BinarySearchTreeLazyTest.this.EnumType[Tree]
          }
        }): (DepEnumType[(Int, Range), Tree], (Int, Range)) => EnumType[Tree])

    val bound = 8

    var count = 0
    for (x <- 1 to bound) {
      val enum = bst(bound-1, 1 to bound)
      var nextInd = 0
      while (nextInd < enum.size) {
        enum.reset
        val t = enum(nextInd)
        val newT = t insert x
        assert( newT.lazyInvariant )

        nextInd = enum.next(nextInd)
        count += 1
      }
    }
    
    println(s"For trees of size $size, enumerated only $count out of total ${bst(bound-1, 1 to bound).size * bound} trees.")
  }

}