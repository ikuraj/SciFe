package scife
package enumeration
package benchmarks

import dependent._
import memoization._
import scife.{ enumeration => e }
import scife.util._

import scife.util.logging._

import structures.riff._
import RiffFormat._

import org.scalatest._
import org.scalameter.api._

import scala.language.existentials

class RiffImage extends StructuresBenchmark[Depend[(Int, Int, Int, Int), RiffFormat.Chunk]] {

  // size, dataSize, totalJiff, avRatio
  type Input = (Int, Int, Int, Int)
  type PInput = ((Int, Int), (Int, Int))
  // list of extends
  type Output = RiffFormat.Chunk
  type UnsOutput = (Int, Int, Int, Int)
  type EnumType = Depend[Input, Output]

  def measureCode(tdEnum: EnumType) = {
    { (size: Int) =>
      val enum = tdEnum.getEnum((size, size, (size + 1) / 2, size/2))
      for (i <- 0 until enum.size) enum(i)
    }
  }

  def warmUp(tdEnum: EnumType, maxSize: Int) {
    for (size <- 1 to maxSize) {
      val enum = tdEnum.getEnum((size, size, (size + 1) / 2, size/2))
      for (i <- 0 until enum.size) enum(i)
    }
  }

  def constructEnumerator(implicit ms: MemoizationScope) = {

     Depend.memoized(
      (self: Depend[Input, Output], in: Input) => {
        // size of the structure, payload size, #jiffed chunks, #audio chunks
        val (size, dataSize, jiffLoss, avChunks) = in

        if (size == 0 && dataSize == 0) e.Singleton(RiffFormat.Leaf): Finite[Chunk]
        else if (size == 1 && dataSize > 0 && avChunks <= 1 && (jiffLoss * 4) % dataSize == 0)
          e.Singleton( Payload(dataSize, jiffLoss * 4 / dataSize, avChunks) ): Finite[Chunk]
        else if (size > 1 && dataSize > 0) {
          val leftSizes = e.Enum(0 until size-1)
          val rootLeftPairs1 = e.dependent.Chain(leftSizes,
            Depend({ (x: Int) => Enum(
              math.max(0, avChunks - (size - x - 1)) to math.min(x, avChunks))
            } ))

          val leftDataSizes = e.Enum(0 to dataSize/2)
          val rootLeftPairs2: Enum[(Int, Int)] = e.dependent.Chain(leftDataSizes,
            Depend({ (x: Int) => Enum(
              math.max(0, jiffLoss - (dataSize - x)) to math.min(x, jiffLoss))
          } ))

          
          val rootLeftPairs = e.Product(rootLeftPairs1, rootLeftPairs2)

          val leftTrees: Depend[PInput, Output] = InMap(self, { (in: PInput) =>
            val ((leftSize, leftAudio), (leftData, leftJiff)) = in
            (leftSize, leftData, leftJiff, leftAudio)
          })

          val rightTrees: Depend[PInput, Output] = InMap(self, { (in: PInput) =>
            val ((leftSize, leftAudio), (leftData, leftJiff)) = in
            (size - leftSize - 1, dataSize - leftData,
              jiffLoss - leftJiff, avChunks - leftAudio)
          })
          
          val leftRightTreePairs: Depend[PInput, (Output, Output)] =
            Product(leftTrees, rightTrees)

          val allNodes =
            memoization.Chain[PInput, (Output, Output), Output](rootLeftPairs, leftRightTreePairs,
              (p1: PInput, p2: (Output, Output)) => Node(dataSize, p2._1, p2._2)
            )

          allNodes: Finite[Chunk]
        } else e.Empty
      })
  }

}
