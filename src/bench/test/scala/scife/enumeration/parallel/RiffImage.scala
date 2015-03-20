package scife
package enumeration
package parallel

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
import org.scalameter.api._

import scala.language.existentials

class RiffImage(numberOfThreads: Int)
  extends StructuresBenchmark[Depend[(Int, Int, Int, Int), RiffFormat.Chunk]] {

  // size, dataSize, totalJiff, avRatio
  type Input = (Int, Int, Int, Int)
  type PInput = ((Int, Int), (Int, Int))
  // list of extends
  type Output = RiffFormat.Chunk
  type UnsOutput = (Int, Int, Int, Int)
  type EnumType = Depend[Input, Output]

  def measureCode(tdEnum: EnumType) = {
    { (size: Int) =>
//      println("RUUUUUUUUUUUUUN")
      this.tdEnum = tdEnum
      this.size = size

      initExecutor
      exec.invokeAll(runners)
//      exec.shutdown()
//      println("EEEEEEEEEEEEEEEEEEND")
//      System.out.flush
    }
  }

  var size: Int = _
  var tdEnum: EnumType = _

  @transient
  var exec: ExecutorService = _

  def initExecutor = exec = Executors.newFixedThreadPool(numberOfThreads)

  var i = 0
  @transient
  lazy val runners: java.util.Collection[Callable[Object]] =
//_
//  def initRunners = runners =
{
    val al = new java.util.ArrayList[Callable[Object]]()
    i = 0
    while (i < numberOfThreads) {
      al add Executors.callable(new Runnable {
        val myInd = i
        
        def run = {
          try {
//            var myInd = Thread.currentThread().getId.toInt
            println(s"my id is $myInd")
            val enum = tdEnum.getEnum((size, size, (size + 1) / 2, size/2))
  
            var ind = myInd
            while (ind < enum.size) {
              enum(ind)
              ind += numberOfThreads
            }
            
          } catch {
            case t: Throwable =>
              println(s"Thrown $t:${t.getStackTrace.mkString("\n")} at $i")
          }
        }
      })
      i+=1
    }
//    println(s"list has ${al.size}")
    if (al.size != numberOfThreads) throw new RuntimeException("al.size")
    
    al
  }
  
  var freeMemory: Int = _
  import scala.scife.enumeration.util._

  override def setUp(size: Int, tdEnum: EnumType, memScope: e.memoization.MemoizationScope) {
//    import Memory._
//
//    getFreeMemory
//    println(s"Freeing up $freeMemory")
//    tryToFreeUpSpaceG(freeMemory)    
//    getFreeMemory
    runners != null
//    Thread.sleep(runners.size)
//    System.gc;
//    System.gc;
//    Thread.sleep(1000)
//    System.gc;
//    System.gc;
//    Thread.sleep(1000)
  }

  def warmUp(tdEnum: EnumType, maxSize: Int) {
    
    for (size <- 1 to maxSize) {
      val enum = tdEnum.getEnum((size, size, (size + 1) / 2, size/2))
      for (i <- 0 until enum.size) enum(i)
    }

//    freeMemory = Memory.getFreeMemory

//    initExecutor
//    this.tdEnum = tdEnum
//    this.size = 5
//
//    initExecutor
//    exec.invokeAll(runners)
//    exec.shutdown()

//    initExecutor
//    exec.awaitTermination(10, TimeUnit.SECONDS)
//    import Memory._
//
//    getFreeMemory
//    println(s"Freeing up $freeMemory")
//    tryToFreeUpSpaceG(freeMemory)    
//    getFreeMemory
  }
  
  override def tearDown(i: Int, tdEnum: EnumType, memScope: e.memoization.MemoizationScope): Unit = {
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
            
            if (size < 15)
              e.memoization.Chain[PInput, (Output, Output), Output](rootLeftPairs, leftRightTreePairs,
                (p1: PInput, p2: (Output, Output)) => Node(dataSize, p2._1, p2._2)
              )
            else
              e.dependent.Chain[PInput, (Output, Output), Output](rootLeftPairs, leftRightTreePairs,
                (p1: PInput, p2: (Output, Output)) => Node(dataSize, p2._1, p2._2)
              )

          allNodes: Finite[Chunk]
        } else e.Empty
  }

  def constructEnumerator(implicit ms: e.memoization.MemoizationScope) = {
    val enum = Depend.memoizedConcurrentNoScope(enumeratorFunction)
    ms.add(enum)
    enum
  }

}