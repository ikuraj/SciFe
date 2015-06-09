package scife
package enumeration
package benchmarks
package parallel
package test

import dependent._
import memoization._
import scife.{ enumeration => e }
import e.parallel._
import scife.util._
import scife.util.logging._
import java.util.concurrent._
import benchmarks._
import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._
import scala.language.existentials
import scife.enumeration.memoization.scope.AccumulatingScope

class BinarySearchTree extends FunSuite {

  import structures._
  import BSTrees._

  type EnumType = Depend[(Int, Range), Tree]

  val count = new java.util.concurrent.atomic.AtomicInteger(0)
  var beg = System.currentTimeMillis()
  var end = System.currentTimeMillis()

  test("measure code") {

    val scope = new AccumulatingScope
    this.tdEnum = constructEnumerator(scope)
    val size = 14

    for (size <- 1 to size) {
      val enum = tdEnum.getEnum((size, 1 to size))
      for (i <- 0 until enum.size) enum(i)
    }
    scope.clear

    //      for (numOfThreads <- 2 to Runtime.getRuntime.availableProcessors) {
    val numOfThreads = 2
    this.size = size

    initExecutor(numOfThreads)
    val runnerList = runners(numOfThreads)
    beg = System.currentTimeMillis()
    exec.invokeAll(runnerList)
    end = System.currentTimeMillis()
    exec.shutdown()

    println(s"size=$size, threads=$numOfThreads, time=${end - beg}")
    scope.clear
    //      }

    //      println("EEEEEEEEEEEEEEEEEEND")
    //      System.out.flush
  }

  private[this] var size: Int = _
  @volatile
  private[this] var tdEnum: EnumType = _

  private[this] var exec: ExecutorService = _ //Executors.newFixedThreadPool(numberOfThreads)

  def initExecutor(numberOfThreads: Int) = exec = Executors.newFixedThreadPool(numberOfThreads)

  var i = 0
  def runners(numberOfThreads: Int): java.util.Collection[Callable[Object]] =
    {
      val al = new java.util.ArrayList[Callable[Object]]()
      i = 0
      while (i < numberOfThreads) {
        al add Executors.callable(new Runnable {
          val myInd = i
          val increment = numberOfThreads

          def run = {
            //          try {
            //            var myInd = Thread.currentThread().getId.toInt
            println(s"my id is $myInd, my incrmeent $increment, tdEnum=${tdEnum.hashCode()}")
            val s = size
            //            for (s <- 1 to size) {
            val enum = tdEnum.getEnum((s, 1 to s))

            var ind = myInd
            while (ind < enum.size) {
              enum(ind)
              ind += increment
              //            }
            }

            //          } catch {
            //            case t: Throwable =>
            //              println(s"Thrown $t:${t.getStackTrace.mkString("\n")} at $i")
            //          }
            //            val cnt = count.incrementAndGet()
            //            if (cnt == numberOfThreads-1) {
            //              end = System.currentTimeMillis()
            //              println(s"size=$size, threads=$numberOfThreads, time=${end-beg}")
            //              throw new RuntimeException
            //            } else {
            //              while (true) {}
            //            }
          }
        })
        i += 1
      }
      //    println(s"list has ${al.size}")
      if (al.size != numberOfThreads) throw new RuntimeException("al.size")

      al
    }

  def tearDown(i: Int, tdEnum: EnumType, memScope: e.memoization.MemoizationScope): Unit = {
    exec.shutdown()
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

        memoization.Chain.breadthSearchPar[(Int, Int), (Tree, Tree), Node](rootLeftSizePairs, leftRightPairs,
          (p1: (Int, Int), p2: (Tree, Tree)) => {
            val ((leftSize, currRoot), (leftTree, rightTree)) = (p1, p2)

            Node(leftTree, currRoot, rightTree)
          })

      }
    }

  def constructEnumerator(implicit ms: e.memoization.MemoizationScope) = {
    val enum = Depend.memoizedConcurrentOptNoScope(enumeratorFunction)
    ms.add(enum)
    enum
  }

}