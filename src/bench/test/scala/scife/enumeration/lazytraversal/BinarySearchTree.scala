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

class BinarySearchTree
  extends StructuresBenchmark[Depend[((Int, Range), LazyEnum[Tree]), Tree] {
  type EnumSort[A] = Finite[A] with Touchable[A] with Resetable[A] with Skippable[A] }]
//  extends PerformanceTest.OfflineReport with ProfileLogger
  {

  type Ugly = LazyEnum[Tree]
  type EnumType[A] = Finite[A] with Touchable[A] with Resetable[A] with Skippable[A]
  type DepEnumType[I, O] = Depend[I, O] { type EnumSort[A] = BinarySearchTree.this.EnumType[A] }
  type DepEnumTypeFinite[I, O] = DependFinite[I, O] { type EnumSort[A] = BinarySearchTree.this.EnumType[A] }

  type EType = DepEnumType[((Int, Range), LazyEnum[Tree]), Tree]

  implicit val treeTag = implicitly[reflect.ClassTag[scife.util.structures.LazyBSTrees.Tree]]

//  override def generator(maxSize: Int): Gen[(Int, Int)] =
//    for (size <- Gen.range("size")(1, maxSize, 1);
//      missingEl <- Gen.range("missingElement")(0, size - 1, 1)) yield
//      (size, missingEl)
//      
  def measureCode(tdEnum: EType) = {
    (size: Int) =>
      var enum = tdEnum.getEnum((size - 1, 1 to size - 1), null)
      for (el <- 1 to size) {
        enum = tdEnum.getEnum((size - 1, 1 to size - 1), null)
        var nextInd = 0
        while (nextInd < enum.size) {
          enum.reset
          val t = enum(nextInd)
          val index = LazyBSTrees.insert(t, el)
          index.lazyInvariant
          nextInd = enum.next(nextInd)
        }
      }
  }

  def warmUp(inEnum: EType, maxSize: Int) {
    for (size <- 1 to maxSize) {
      val enum = inEnum.getEnum((size, 1 to size), null)
      for (i <- 0 until enum.size) enum(i)
    }
  }

  def constructEnumerator(implicit ms: e.memoization.MemoizationScope): DepEnumType[((Int, Range), LazyEnum[Tree]), Tree] = {
    new WrapFunctionTest2[((Int, Range), Ugly), Tree, EnumType](
      ((self: DepEnumType[((Int, Range), Ugly), Tree], pair: ((Int, Range), Ugly)) => {
        val ((size, range), ug) = pair

        val reuse: split.ChainFiniteSingleCombine[(Int, Int), LazyTuple2[Tree, Tree], Tree] =
          if (ug.isInstanceOf[split.ChainFiniteSingleCombine[(Int, Int), LazyTuple2[Tree, Tree], Tree]]) {
            ug.asInstanceOf[split.ChainFiniteSingleCombine[(Int, Int), LazyTuple2[Tree, Tree], Tree]]
          } else null

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
            ((leftSize, range.start to (median - 1)), ug)
          }) with DependFinite[(Int, Int), Tree] {
            override type EnumSort[A] = BinarySearchTree.this.EnumType[A]
          }

          val rightTrees: DepEnumTypeFinite[(Int, Int), Tree] =
            new InMap(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              ((size - leftSize - 1, (median + 1) to range.end), ug)
            }) with DependFinite[(Int, Int), Tree] {
              override type EnumSort[A] = BinarySearchTree.this.EnumType[A]
            }

          val leftRightPairs: DepEnumTypeFinite[(Int, Int), LazyTuple2[Tree, Tree]] =
            lazytraversal.split.dependent.ProductFinite(leftTrees, rightTrees)

          val fConstructTree: ((Int, Int), => LazyTuple2[Tree, Tree]) => Tree =
            (p1, p2) => {
              Node(p2._1, p1._2, p2._2)
            }

          val allNodes =
            if (reuse == null)
              new {
                val classTagT = treeTag
              } with lazytraversal.split.ChainFiniteSingleCombine[(Int, Int), LazyTuple2[Tree, Tree], Tree](
                rootLeftSizePairs, leftRightPairs,
                fConstructTree)(null) with e.memoization.MemoizedSize with e.memoization.MemoizedStatic[Tree] with Touchable[Tree] {
                //            override def toString = s"ChainFiniteSingleCombine[$hashCode](${leftRightPairs.hashCode})"
              }
            else
              new {
                val classTagT = treeTag
              } with lazytraversal.split.ChainFiniteSingleCombine[(Int, Int), LazyTuple2[Tree, Tree], Tree](
                rootLeftSizePairs, leftRightPairs,
                fConstructTree)(reuse.inner) with e.memoization.MemoizedSize with e.memoization.MemoizedStatic[Tree] with Touchable[Tree] {
                //            override def toString = s"ChainFiniteSingleCombine[$hashCode](${leftRightPairs.hashCode})"
              }

          allNodes: BinarySearchTree.this.EnumType[Tree]
        }
      }): (DepEnumType[((Int, Range), Ugly), Tree], ((Int, Range), Ugly)) => EnumType[Tree]) with split.Memoized[(Int, Range), Tree, Ugly]
  }

}
