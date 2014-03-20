package insynth
package streams
package benchmarks

import org.scalatest._
import org.scalameter.api._

import dependent._
import streams.{ light => e }

import util._
import util.logging._
import Structures.BSTrees._

class HeapArrayBenchmark extends DependentMemoizedBenchmark[Int, Dependent[(Int, List[Int]), Tree]]
  with java.io.Serializable with HasLogger {
  import common._
  import e.Enum

  val maxSize = 10

  fixture

  type EnumType = Dependent[(Int, List[Int]), Tree]

  override def name = "HeapArray"

  def measureCode(using: super.Using[Int], tdEnum: EnumType) = {
    using in { (size: Int) =>
      val enum = tdEnum.getStream((size, rangeList(size)))
      val elements =
        for ( ind <- 0 until enum.size ) yield enum(ind)
    }
  }

  def generator = Gen.range("size")(1, maxSize, 1)

  def warmUp(inEnum: EnumType) {
    val tdEnum = inEnum.asInstanceOf[EnumType]
    for (size <- 1 to maxSize) {
      val enum= tdEnum.getStream((size, rangeList(size)))
      val elements =
        for (
          ind <- 0 until enum.size
        ) yield enum(ind)
    }
  }
  
  def rangeList(m: Int) = m to 0 by -1 toList

  def constructEnumerator(ms: MemoizationScope) = {
    Producer.memoized(
      (self: EnumType, pair: (Int, List[Int])) => {
      // list sorted descendingly
      val (size, list) = pair

      if (size <= 0) e.Singleton(Leaf)
      else if (size == 1)
        (e.Enum(list): Enum[Int]) map { v => Node(Leaf, v, Leaf) }
      else if (!list.isEmpty) {
        val rootsInds = Enum(0 until list.size)

        val childHeaps = new InMapper(self, { (rootInd: Int) =>
          ( (size-1)/2, list.drop(rootInd) )
        })
        val leftRightPairs: Dependent[Int, (Tree, Tree)] =
          CoupledBinary(childHeaps, childHeaps)
        
        import BinaryFiniteMemoized._

        val allNodes =
          combine[Int, (Tree, Tree), Node](rootsInds, leftRightPairs,
            (rootInd: Int, p2: (Tree, Tree)) => {
              val (leftTree, rightTree) = p2

              Node(leftTree, list(rootInd), rightTree)
            })

        allNodes
      } else e.Empty
    })
  }

}
