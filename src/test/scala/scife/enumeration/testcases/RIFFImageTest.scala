package scife.enumeration
package testcases

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._

import dependent._
import scife.{ enumeration => e }
import memoization._

import scife.util._
import scife.util.logging._
import structures.BSTrees._

import org.scalatest._
import org.scalameter.api._

import scala.language.postfixOps
import scala.language.existentials

class RiffImageTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
  HasLogger with ProfileLogger {
  import Checks._
  import structures.riff._

  import RiffFormat._
  import Util._
  import Common._

  // size, dataSize, totalJiff, avRatio
  type Input = (Int, Int, Int, Int)
  type PInput = ((Int, Int), (Int, Int))
  // list of extends
  type Output = RiffFormat.Chunk
  type UnsOutput = (Int, Int, Int, Int)
  type EnumType = Depend[Input, Output]

  test("enumeration") {
    val checkerHelper = new CheckerHelper[Output]
    import checkerHelper._

    val enum = constructEnumerator

    withLazyClue("Elements are: " + clue) {
//      res = enum.getEnum((1, 4, 1, 0))
//      res.size should be (1)
//      res(0) shouldBe a [Payload]
//      val ds = res(0).asInstanceOf[Payload]
//      ds.data shouldBe 4 
//      ds.isAudio shouldBe 0
//      ds.jiff shouldBe 1
//      RiffFormat.size(ds) shouldBe 1

//      res = enum.getEnum((2, 4, 1, 1))
//      res.size should be (1)
      
      res = enum.getEnum((2, 2, 1, 1))
      res.size should be (1)
      
      res = enum.getEnum((3, 3, 2, 1))
      res.size should be (4)
      
      res = enum.getEnum((4, 4, 2, 2))
      res.size should be (9)
      
      res = enum.getEnum((5, 5, 3, 2))
//      res foreach println
      res foreach { x => RiffFormat.size(x) shouldBe 5 }
      res.size should be (54)

      res = enum.getEnum((6, 6, 3, 3))
      res.size should be (115)

      res = enum.getEnum((7, 7, 4, 3))
      res.size should be (932)
      
    }

  }

  def constructEnumerator(implicit ms: MemoizationScope = null) = {

    Depend.memoized(
      (self: Depend[Input, Output], in: Input) => {
        // size of the structure, payload size, #jiffed chunks, #audio chunks
        val (size, dataSize, jiffLoss, avChunks) = in

        if (size == 0 && dataSize != 0 ||
            size > 0 && dataSize == 0 ||
            size == 1 && avChunks <= 1 && (jiffLoss * 4) % dataSize != 0) {
          println(s"(size, dataSize, jiffLoss, avChunks)=${(size, dataSize, jiffLoss, avChunks)}")
          e.Empty
        }
        else if (size == 0 && dataSize == 0) {
          avChunks shouldBe 0 
          dataSize shouldBe 0
          jiffLoss shouldBe 0
          e.Singleton(RiffFormat.Leaf): Finite[Chunk]
        }
        else if (size == 1 && avChunks <= 1 && (jiffLoss * 4) % dataSize == 0)
          e.Singleton( Payload(dataSize, jiffLoss * 4 / dataSize, avChunks) ): Finite[Chunk]
        else if (size > 1 && dataSize > 0) {
          val leftSizes = e.Enum(0 until size-1)
          val rootLeftPairs1 = e.dependent.Chain(leftSizes,
            Depend({ (x: Int) => Enum(
              math.max(0, avChunks - (size - x - 1)) to math.min(x, avChunks))
            } ))
//          rootLeftPairs1.size shouldBe > (0)

          val leftDataSizes = e.Enum(0 to dataSize/2)
          val rootLeftPairs2: Enum[(Int, Int)] = e.dependent.Chain(leftDataSizes,
            Depend({ (x: Int) => Enum(
              math.max(0, jiffLoss - (dataSize - x)) to math.min(x, jiffLoss))
          } ))
          rootLeftPairs2.size shouldBe > (0)

//          val rootLeftPairs1 = e.Product(leftSizes, leftDataSizes)
//          val rootLeftPairs2 = e.Product(leftJiffs, leftAudios)
          
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
            // TODO: Input not needed to construct Node
            memoization.Chain[PInput, (Output, Output), Output](rootLeftPairs, leftRightTreePairs,
              (p1: PInput, p2: (Output, Output)) => Node(dataSize, p2._1, p2._2)
            )
//          allNodes.size shouldBe > (0)

          allNodes: Finite[Chunk]
        } else throw new RuntimeException(s"Input:$in")//e.Empty
      })
  }

}
