package scife
package enumeration
package benchmarks

import dependent._
import memoization._
import scife.{ enumeration => e }
import scife.util._

import scife.util.logging._

import structures._
import BTree._

import org.scalatest._
import org.scalameter.api._

import scala.language.existentials
import scala.language.postfixOps

class BTreeTest
  extends StructuresBenchmark[Depend[(Int, Range, Int), BTree.Tree]]
  {

  // size, range of keys
  type Input = (Int, Range, Int)
  type Output = BTree.Tree
  type EnumType = Depend[Input, Output]

  def measureCode(tdEnum: EnumType) = {
    { (size: Int) =>
      for (
        height <- 1 to (math.log(size+1).toInt + 1);
        enum = tdEnum.getEnum(size, 1 to size, height);
        ind <- 0 until enum.size
      ) enum(ind)
    }
  }

  def warmUp(tdEnum: EnumType, maxSize: Int) {
    for (size <- 1 to maxSize) {
      for (
        height <- 1 to (math.log(size+1).toInt + 1);
        enum = tdEnum.getEnum(size, 1 to size, height);
        ind <- 0 until enum.size
      ) enum(ind)
    }
  }

  def getAdditionsEnum(implicit ms: MemoizationScope = null): DependFinite[(Int, Int, Int), List[Int]] = {
    type Input = (Int, Int, Int)

    Depend.memoizedFin(
      (self: DependFinite[Input, List[Int]], pair: Input) => {
        val (size, amount, max) = pair

        if (size == 0 && amount > 0) e.Empty
        else if (size == 0) e.Singleton(Nil)
        else if (size == 1 && amount > max) e.Empty
        else {
          e.dependent.Chain.single(Enum(0 to math.min(amount, max)),
            Depend.fin({ (added: Int) =>
              self(size - 1, amount - added, max) map {
                added :: _
              }
            }))
        }
      })
  }
  
  val t = 2

  def constructEnumerator(implicit ms: MemoizationScope)
//    ct: scala.reflect.ClassTag[Output])
    = {
    implicit val ct = implicitly[scala.reflect.ClassTag[Output]]

    def minChildSize(h: Int) = 2 * math.pow(t, h - 1).toInt - 1
    def maxChildSize(h: Int) = math.pow(2 * t, h).toInt - 1

    val additionsEnum = getAdditionsEnum(ms)

    var nonRootNodeEnum: DependFinite[(Int, Range, Int), Output] = null

    val enumChildren: DependFinite[(Int, Int, Range, Int), Output] = Depend.fin({ in: (Int, Int, Range, Int) =>
      val (nChildren: Int, size: Int, keyRange: Range, h: Int) = in

      val minChildSizeBelow = minChildSize(h - 1)
      val maxChildSizeBelow = maxChildSize(h - 1)
      val nKeys = nChildren - 1
      val restOfNodes: Int = size - nKeys - nChildren * minChildSizeBelow

      if (restOfNodes < 0) {
        e.Empty
      } else {
        e.dependent.Chain.single[List[Int], Output](
          additionsEnum(nChildren, restOfNodes, maxChildSizeBelow - minChildSizeBelow): Finite[List[Int]],
          Depend.fin({ (addList: List[Int]) =>
            val childSizes = addList map { _ + minChildSizeBelow }

            e.dependent.Chain.single[List[Int], Output](
              additionsEnum(nChildren, keyRange.size - size, keyRange.size): Finite[List[Int]],
              Depend.fin({ (addKeys: List[Int]) =>
                val keys: List[Int] = ((childSizes zip addKeys).scanLeft(keyRange.start - 1) {
                  case (soFar, (childSize, add)) => soFar + childSize + add + 1
                }).tail.init
    
                val childRanges =
                  ((keyRange.start - 1 :: keys) zip (keys :+ (keyRange.end + 1))) map {
                    case (a, b) => (a + 1) to (b - 1)
                  }
    
                val childEnums: Array[Finite[Output]] = (childSizes zip childRanges) map {
                  case (cs, cr) => nonRootNodeEnum(cs, cr, h - 1)
                } toArray;
    
                e.Product.fin(childEnums) map {
                  children =>
                    Tree(keys, children)
                }
              }))
          }): DependFinite[List[Int], Output])
      }
    })

    nonRootNodeEnum = Depend.memoizedFin(
      (self: DependFinite[Input, Output], pair: Input) => {
        val (size: Int, keyRange: Range, h: Int) = pair

        if (h == 1 && size < 2 * t && size >= t - 1) {
          e.Sublists(keyRange.toList, size) map { x => Tree(x.toList, Nil) }
        } else if (h > 1 /*&& size == keyRange.size*/ && size > 0) {
          val nChildrenEnum = e.Enum(t to 2 * t)

          e.dependent.Chain.single(nChildrenEnum map { x => (x, size, keyRange, h) },
            enumChildren): Finite[Output]
        } else {
          e.Empty //throw new RuntimeException(s"input=$pair")
        }
      })

    val rootNodeEnum = Depend.memoizedFin(
      (self: DependFinite[Input, Output], pair: Input) => {
        val (size: Int, keyRange: Range, h: Int) = pair

        if (h == 1 && size < 2 * t) {
          e.Sublists(keyRange.toList, size) map { x => Tree(x.toList, Nil) }
        } else
          // uncomenting size >= 2*t will actually prevent some trees that cannot happen by insertion?
          if (h > 1 /*&& size == keyRange.size && size >= 2 * t*/) {
          val nChildrenEnum = e.Enum(2 to 2 * t)

          e.dependent.Chain.single(nChildrenEnum map { x => (x, size, keyRange, h) },
            enumChildren): Finite[Output]
        } else {
          info(s"input at root=$pair")
          e.Empty //throw new RuntimeException(s"input=$pair")
        }
      })

    rootNodeEnum
  }


}
