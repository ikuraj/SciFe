package scife
package enumeration
package lazytraversal

import scife.enumeration.dependent._
import memoization._
import scife.{ enumeration => e }
import scife.util._

import scife.util.logging._

import scife.util.structures._
import LazyBSTrees._

import scalaz.LazyTuple2

import benchmarks._

import org.scalatest._
import org.scalameter.api._

import scala.language.existentials

class BinarySearchTreeBenchmark
  extends DependentMemoizedBenchmark[(Int, Int), Depend[(Int, Range), Tree]] //  extends PerformanceTest.OfflineReport with ProfileLogger
  {

  type EnumType[A] = Finite[A] with Touchable[A] with Resetable[A] with Skippable[A]
  type DepEnumType[I, O] = Depend[I, O] { type EnumSort[A] = BinarySearchTreeBenchmark.this.EnumType[A] }
  type DepEnumTypeFinite[I, O] = DependFinite[I, O] { type EnumSort[A] = BinarySearchTreeBenchmark.this.EnumType[A] }

  type EType = Depend[(Int, Range), Tree]

  override def generator(maxSize: Int): Gen[(Int, Int)] =
    for (size <- Gen.range("size")(1, maxSize, 1);
      missingEl <- Gen.range("missingElement")(0, size - 1, 1)) yield
      (size, missingEl)
      
  def measureCode(tdEnum: EType) = {
    { (in: (Int, Int)) =>
      val (size, el) = in
      val enum = tdEnum.getEnum((size, 1 to size))
      for (i <- 0 until enum.size) {
//        val t = enum(i)
//        val index = LazyBSTrees.insert(t, e)
//        index.
      }
    }
  }

  def warmUp(inEnum: EType, maxSize: Int) {
    for (size <- 1 to maxSize) {
      val enum = inEnum.getEnum((size, 1 to size))
      for (i <- 0 until enum.size) enum(i)
    }
  }

  override def constructEnumerator(implicit ms: e.memoization.MemoizationScope) = {
    new WrapFunctionTest[(Int, Range), Tree, EnumType](
      ((self: DepEnumType[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        if (size < 0) throw new RuntimeException
        if (size <= 0) new e.Singleton((Leaf: Tree)) with Touchable[Tree] with Resetable[Tree] with NoSkip[Tree] {
          override def toString = s"Singleton[$hashCode]"
        }
        else if (range.isEmpty) Empty
        else if (size == 1)
          new e.WrapArray(range map { v => (Node(Leaf, v, Leaf): Tree) } toArray) with
            Touchable[Tree] with Resetable[Tree] with NoSkip[Tree]
        else {
          val leftSizes = e.Enum(0 until size)
          val roots = e.Enum(range)

          val rootLeftSizePairs = e.Product(leftSizes, roots)

          val leftTrees: DepEnumTypeFinite[(Int, Int), Tree] = new InMap(self, { (par: (Int, Int)) =>
            val (leftSize, median) = par
            (leftSize, range.start to (median - 1))
          }) with DependFinite[(Int, Int), Tree] {
            override type EnumSort[A] = BinarySearchTreeBenchmark.this.EnumType[A]
          }

          val rightTrees: DepEnumTypeFinite[(Int, Int), Tree] =
            new InMap(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (size - leftSize - 1, (median + 1) to range.end)
            }) with DependFinite[(Int, Int), Tree] {
              override type EnumSort[A] = BinarySearchTreeBenchmark.this.EnumType[A]
            }

          val leftRightPairs: DepEnumTypeFinite[(Int, Int), LazyTuple2[Tree, Tree]] =
            lazytraversal.dependent.ProductFinite(leftTrees, rightTrees)

          val fConstructTree: ((Int, Int), => LazyTuple2[Tree, Tree]) => Tree =
            (p1, p2) => {
              Node(p2._1, p1._2, p2._2)
            }

          val allNodes =
            new lazytraversal.ChainFiniteSingleCombine[(Int, Int), LazyTuple2[Tree, Tree], Tree](
              rootLeftSizePairs, leftRightPairs,
              fConstructTree) with Touchable[Tree]

          allNodes: EnumType[Tree]
        }
      }): (DepEnumType[(Int, Range), Tree], (Int, Range)) => EnumType[Tree])
  }

}
