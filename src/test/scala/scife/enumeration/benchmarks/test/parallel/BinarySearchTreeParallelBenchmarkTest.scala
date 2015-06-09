package scife
package enumeration
package benchmarks
package test
package parallel

import dependent._
import memoization._
import scife.{ enumeration => e }
import scife.util._

import scife.util.logging._

import java.util.concurrent._

import structures._
import BSTrees._

import org.scalatest._
import org.scalatest.prop._
import org.scalameter.api._

import scala.language.existentials

class BinarySearchTreeParallelBenchmarkTest
  extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {

  type EnumType = Depend[(Int, Range), Tree]

  val queue = new LinkedTransferQueue[Int]

  val NumberOfProcessors = Runtime.getRuntime.availableProcessors
  info(s"NumberOfProcessors=$NumberOfProcessors")

  test("correctness of concurrent execution") {

    import scife.enumeration.parallel.memoization.scope._

    for (numberOfThreads <- NumberOfProcessors to NumberOfProcessors; size <- 4 to 7) {
      val ms = new AccumulatingConcurrentScope
      val tdEnum = constructEnumerator(ms)
      tdEnum shouldBe a[enumeration.parallel.memoization.dependent.Memoized[_, _]]

      val ms2 = new scope.AccumulatingScope
      val origTdEnum = common.enumdef.BinarySearchTreeEnum.constructEnumeratorBenchmark(ms2)

      val exec = Executors.newFixedThreadPool(numberOfThreads)

      val allEnumerated = new java.util.concurrent.ConcurrentLinkedQueue[Tree]()
      val threadsFinished = new java.util.concurrent.atomic.AtomicInteger(0)

      //      for (i <- 0 until numberOfThreads)
      //        exec.execute(new Runnable {
      //          def run = {
      //            var mySize = i
      //            while (mySize <= size) {
      //              tdEnum.getEnum((mySize, 1 to mySize))
      //              mySize += numberOfThreads
      //            }
      //
      //            mySize = size
      //            var myInd = i
      ////            while (mySize <= size) {
      //              val enum = tdEnum.getEnum((mySize, 1 to mySize))
      //              enum.size shouldBe origTdEnum.getEnum((mySize, 1 to mySize)).size
      //              while (myInd < enum.size) {
      //                val tree = enum(myInd)
      //                if (mySize == size) allEnumerated.add(tree)
      //                myInd += numberOfThreads
      //              }
      ////
      ////              mySize += 1
      ////            }
      //            
      //            threadsFinished.incrementAndGet()
      //            println("I'm out")
      //          }
      //        })
      //        
      import scala.collection.JavaConversions._

      val runners =
        for (i <- 0 until numberOfThreads)
          yield new Runnable {
          def run = {
            try {
              var myInd = i
              val enum = tdEnum.getEnum((size, 1 to size))
              enum.size shouldBe >(i)
              // concurrency bugs in origTdEnum
              //enum.size shouldBe origTdEnum((size, 1 to size)).size
              while (myInd < enum.size) {
                //info(s"Thread $i myInd=$myInd, enum.size=${enum.size}")
                val tree = enum(myInd)
                allEnumerated.add(tree)
                myInd += numberOfThreads
              }

              threadsFinished.incrementAndGet()
            } catch {
              case t: Throwable =>
                println(s"Throwed $t:${t.getStackTrace.mkString("\n")} at $i")
            }
          }
        }

      exec.invokeAll(runners map { r => Executors.callable(r) })
      exec.shutdown()
      //      exec.awaitTermination(10, TimeUnit.MINUTES)
      threadsFinished.get shouldBe numberOfThreads

      val origEnum = origTdEnum((size, 1 to size))
      allEnumerated.size shouldBe origEnum.size

      withClue(
        s"allEnumerated=\n${allEnumerated.toList.mkString("\n")}" +
        s"origEnum=\n${origEnum.toList.mkString("\n")}"
      ) {
        val allEnumeratedSet = allEnumerated.toSet
        for (i <- 0 until origEnum.size)
          allEnumeratedSet should contain (origEnum(i))
        ms.clear
        ms2.clear
      }

    }
  }
  
  test("correctness of concurrent execution, without scope") {

    import scife.enumeration.parallel.memoization.scope._

    for (numberOfThreads <- NumberOfProcessors to NumberOfProcessors; size <- 5 to 7) {
      val tdEnum = constructEnumeratorNoScope
      tdEnum shouldBe a[enumeration.memoization.Memoizable]

      val ms2 = new scope.AccumulatingScope
      val origTdEnum = common.enumdef.BinarySearchTreeEnum.constructEnumeratorBenchmark(ms2)

      val exec = Executors.newFixedThreadPool(numberOfThreads)

      val allEnumerated = new java.util.concurrent.ConcurrentLinkedQueue[Tree]()
      val threadsFinished = new java.util.concurrent.atomic.AtomicInteger(0)

      import scala.collection.JavaConversions._

      val runners =
        for (i <- 0 until numberOfThreads)
          yield new Runnable {
          def run = {
            try {
              var myInd = i
              val enum = tdEnum.getEnum((size, 1 to size))
              enum.size shouldBe >(i)
              // concurrency bugs in origTdEnum
              //enum.size shouldBe origTdEnum((size, 1 to size)).size
              while (myInd < enum.size) {
                //info(s"Thread $i myInd=$myInd, enum.size=${enum.size}")
                val tree = enum(myInd)
                allEnumerated.add(tree)
                myInd += numberOfThreads
              }

              threadsFinished.incrementAndGet()
            } catch {
              case t: Throwable =>
                println(s"Throwed $t:${t.getStackTrace.mkString("\n")} at $i")
            }
          }
        }

      exec.invokeAll(runners map { r => Executors.callable(r) })
      exec.shutdown()
      threadsFinished.get shouldBe numberOfThreads

      val origEnum = origTdEnum((size, 1 to size))
      allEnumerated.size shouldBe origEnum.size

      tdEnum.asInstanceOf[enumeration.memoization.Memoizable].clearMemoization
      ms2.clear
      val allEnumeratedSet = allEnumerated.toSet
      for (i <- 0 until origEnum.size)
        allEnumeratedSet should contain (origEnum(i))

    }
  }

  val enumeratorFunction =
    (self: Depend[(Int, Range), Tree], pair: (Int, Range)) => {
      val (size, range) = pair

      if (size <= 0) e.Singleton(Leaf)
      else if (size == 1)
        e.WrapArray(range map { v => Node(Leaf, v, Leaf) })
      else {
        val roots = e.Enum(range)
        val leftSizes = e.Enum(0 until size)

        val rootLeftSizePairs = e.Product(leftSizes, roots)

        val leftTrees: Depend[(Int, Int), Tree] = InMap(self, { (par: (Int, Int)) =>
          val (leftSize, median) = par
          (leftSize, range.start to (median - 1))
        })

        val rightTrees: Depend[(Int, Int), Tree] =
          InMap(self, { (par: (Int, Int)) =>
            val (leftSize, median) = par
            (size - leftSize - 1, (median + 1) to range.end)
          })

        val leftRightPairs: Depend[(Int, Int), (Tree, Tree)] =
          Product(leftTrees, rightTrees)

        val allNodes =
          memoization.Chain[(Int, Int), (Tree, Tree), Node](rootLeftSizePairs, leftRightPairs,
            (p1: (Int, Int), p2: (Tree, Tree)) => {
              val ((leftSize, currRoot), (leftTree, rightTree)) = (p1, p2)

              Node(leftTree, currRoot, rightTree)
            })

        allNodes
      }
    }

  def constructEnumerator(implicit ms: MemoizationScope) =
    Depend.memoizedConcurrent(enumeratorFunction)
    
  def constructEnumeratorNoScope =
    Depend.memoizedConcurrentNoScope(enumeratorFunction)

}