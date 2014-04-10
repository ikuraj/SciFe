package insynth
package enumeration
package benchmarks

import dependent._
import insynth.{ enumeration => e }
import memoization._

import insynth.util._
import insynth.util.logging._
import Structures.BSTrees._

import org.scalatest._
import org.scalameter.api._

import scala.language.postfixOps
import scala.language.existentials

class HeapArrayBenchmark
  extends StructuresBenchmark[Depend[(Int, List[Int]), Tree]]
//  extends DependentMemoizedBenchmark[Int, Depend[(Int, List[Int]), Tree]]
  with java.io.Serializable with HasLogger {
  import common._
  import e.Enum

  val maxSize = 10

  fixture

  type EnumType = Depend[(Int, List[Int]), Tree]

  override def name = "HeapArray"

  def measureCode(using: super.Using[Int], tdEnum: EnumType) = {
    using in { (size: Int) =>
      val enum = tdEnum.getEnum((size, rangeList(size)))
      val elements =
        for ( ind <- 0 until enum.size ) yield enum(ind)
    }
  }

//  def generator = Gen.range("size")(1, maxSize, 1)

  def warmUp(inEnum: EnumType) {
    val tdEnum = inEnum.asInstanceOf[EnumType]
    for (size <- 1 to maxSize) {
      val enum= tdEnum.getEnum((size, rangeList(size)))
      val elements =
        for (
          ind <- 0 until enum.size
        ) yield enum(ind)
    }
  }
  
  def rangeList(m: Int) = m to 0 by -1 toList

  def constructEnumerator(ms: MemoizationScope) = {
    Depend.memoized(
      (self: EnumType, pair: (Int, List[Int])) => {
      // list sorted descendingly
      val (size, list) = pair

      if (size <= 0) e.Singleton(Leaf)
      else if (size == 1)
        (e.Enum(list): Enum[Int]) map { v => Node(Leaf, v, Leaf) }
      else if (!list.isEmpty) {
        val rootsInds = Enum(0 until list.size)

        val childHeaps = InMap(self, { (rootInd: Int) =>
          ( (size-1)/2, list.drop(rootInd) )
        })
        val leftRightPairs: Depend[Int, (Tree, Tree)] =
          Product(childHeaps, childHeaps)
        
        val allNodes =
          memoization.Chain[Int, (Tree, Tree), Node](rootsInds, leftRightPairs,
            (rootInd: Int, p2: (Tree, Tree)) => {
              val (leftTree, rightTree) = p2

              Node(leftTree, list(rootInd), rightTree)
            })

        allNodes
      } else e.Empty
    })
  }

}
