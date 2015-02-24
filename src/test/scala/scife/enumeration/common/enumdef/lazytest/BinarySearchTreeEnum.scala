package scife
package enumeration
package common.enumdef
package lazytest

import scife.{ enumeration => e }
import e.iterable._
import scife.util._
import scala.language.postfixOps
import scife.enumeration.iterable.ResetIter

object BinarySearchTreeEnum {
  
  import structures.LazyBSTrees._
  import e.dependent._
  import memoization._
  
  type ResetIterEnumType[A] = ResetIterFinite[A]/* with Memoized[A]*/
  type DepEnumType[I, O] = Depend[I, O] { type EnumSort[A] = ResetIterEnumType[A] }
 
  val enumDefList =
    List(
      constructEnumerator(_: MemoizationScope)
    ) zip List(
      "constructEnumerator"
    )
  
  // slightly changed constructEnumeratorBenchmark
  def constructEnumerator(implicit ms: MemoizationScope) = {
    new WrapFunctionTest[(Int, Range), Tree, ResetIterEnumType](
      ((self: DepEnumType[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        if (size <= 0) new e.Singleton(Leaf) with ResetIter[Tree]
        else if (size == 1)
          new e.WrapArray(range map { v => Node(Leaf, v, Leaf) } toArray) with ResetIter[Tree] 
        else {
          val roots = 
            if (range.start == 0)
              new IdentitySize(range.size) with ResetIter[Int]
            else new WrapRange(range) with ResetIter[Int]
          
          val leftSizes = 
            if ((0 until size).start == 0)
              new IdentitySize((0 until size).size) with ResetIter[Int]
            else new WrapRange((0 until size)) with ResetIter[Int]

          val rootLeftSizePairs = e.iterable.Product(leftSizes, roots)

          val leftTrees = new InMap(self, { (par: (Int, Int)) =>
            val (leftSize, median) = par
            (leftSize, range.start to (median - 1))
          }) with DependFinite[(Int, Int), Tree] {
            override type EnumSort[A] = ResetIterEnumType[A]
          }

          val rightTrees =
            new InMap(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (size - leftSize - 1, (median + 1) to range.end)
            }) with DependFinite[(Int, Int), Tree] {
              override type EnumSort[A] = ResetIterEnumType[A]
            }

          val leftRightPairs =
            new iterable.dependent.ProductFinite(leftTrees, rightTrees) {
              override type EnumSort[A] = ResetIterEnumType[A]
            }

          val allNodes: ResetIterEnumType[Tree] =
            iterable.Chain[(Int, Int), (Tree, Tree), Node](rootLeftSizePairs, leftRightPairs,
              (p1: (Int, Int), p2: (Tree, Tree)) => Node(p2._1, p1._2, p2._2)
            )

          allNodes
        }
      }): (DepEnumType[(Int, Range), Tree], (Int, Range)) => ResetIterEnumType[Tree]
    ) with DependFinite[(Int, Range), Tree] { override type EnumSort[A] = ResetIterEnumType[A] }
  }
  
}
