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
import structures.riff._
import RiffFormat._
import benchmarks._
import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._
import scala.language.existentials
import scife.enumeration.memoization.scope.AccumulatingScope

class RiffImage extends FunSuite with HasLogger {

  // size, dataSize, totalJiff, avRatio
  type Input = (Int, Int, Int, Int)
  type PInput = ((Int, Int), (Int, Int))
  // list of extends
  type Output = RiffFormat.Chunk
  type UnsOutput = (Int, Int, Int, Int)
  type EnumType = Depend[Input, Output]
  
  val count = new java.util.concurrent.atomic.AtomicInteger(0)
  var beg = System.currentTimeMillis()
  var end = System.currentTimeMillis()

  test("measure code") {
    
    
    val scope = new AccumulatingScope
    this.tdEnum = constructEnumerator(scope)
    val size = 14
    
    for (size <- 1 to size) {
      val enum = tdEnum.getEnum((size, size, (size + 1) / 2, size/2))
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
        
        info(s"size=$size, threads=$numOfThreads, time=${end-beg}")
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
            info(s"my id is $myInd, my incrmeent $increment, tdEnum=${tdEnum.hashCode()}")
            val s = size
//            for (s <- 1 to size) {
            val enum = tdEnum.getEnum((s, s, (s + 1) / 2, s/2))
  
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
      i+=1
    }
//    println(s"list has ${al.size}")
    if (al.size != numberOfThreads) throw new RuntimeException("al.size")
    
    al
  }
  
  def tearDown(i: Int, tdEnum: EnumType, memScope: e.memoization.MemoizationScope): Unit = {
    exec.shutdown()
  }

  val enumeratorFunction =
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
            
//            if (size < 15)
//              e.memoization.Chain[PInput, (Output, Output), Output](rootLeftPairs, leftRightTreePairs,
//                (p1: PInput, p2: (Output, Output)) => Node(dataSize, p2._1, p2._2)
//              )
//            else
//              e.dependent.Chain[PInput, (Output, Output), Output](rootLeftPairs, leftRightTreePairs,
//                (p1: PInput, p2: (Output, Output)) => Node(dataSize, p2._1, p2._2)
//              )

//            e.parallel.memoization.Chain.breadthSearch[PInput, (Output, Output), Output](rootLeftPairs, leftRightTreePairs,
//              (p1: PInput, p2: (Output, Output)) => Node(dataSize, p2._1, p2._2)
//            )
            
            memoization.Chain.breadthSearchPar[PInput, (Output, Output), Output](rootLeftPairs, leftRightTreePairs,
              (p1: PInput, p2: (Output, Output)) => Node(dataSize, p2._1, p2._2)
            )

          allNodes: Finite[Chunk]
        } else e.Empty
  }

  def constructEnumerator(implicit ms: e.memoization.MemoizationScope) = {
    // TODO the opt one freezes the program
    val enum = Depend.memoizedConcurrentNoScope(enumeratorFunction)
//    val enum = Depend.memoizedConcurrentOptNoScope(enumeratorFunction)

    ms.add(enum)
    enum
  }

}