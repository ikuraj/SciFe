package scife.enumeration
package parallel

import java.util.concurrent._
import atomic._

import util._
import scife.util.logging._
import scife.util._

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._

import scala.language.existentials

class ConcurrencyTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
  HasLogger with ProfileLogger {

  val NumberOfProcessors = Runtime.getRuntime.availableProcessors
  
  ignore("number of processors") {
    NumberOfProcessors shouldBe 8
  }
  
  private final val exec = Executors.newFixedThreadPool(NumberOfProcessors);
  
  test("simple concurrent printing") {
    
    val count = new AtomicInteger
    
    for (_ <- 1 to NumberOfProcessors)
      exec.execute(new Runnable {
        def run() = {
          val myVal = count.incrementAndGet
          
          info(s"Got count=$count at thread ${Thread.currentThread.getId}")                
        }
      })
    
    Thread.sleep(500)
  }
  
  test("queuing tasks, shutdown explicit") {
    val exec = Executors.newFixedThreadPool(NumberOfProcessors)
    val queue = new LinkedTransferQueue[Int]
    for (_ <- 1 to NumberOfProcessors - 1)
      queue.add(1)
   
    for (_ <- 1 to NumberOfProcessors - 1)
      exec.execute(new Runnable {
        def run() = {
          while (!Thread.interrupted()) {
            val myVal = queue.remove()
            info(s"Got val=$myVal at thread ${Thread.currentThread.getId}")                
          
            val mod = myVal % 2
            if (mod == 1) {
              queue add myVal + 2
            } else queue add myVal + 3
          }
        }
      })
    
    while (queue.peek < 50) { }

    exec.shutdownNow
    
  }

  test("queuing tasks, no more work shut down") {
    val exec = Executors.newFixedThreadPool(NumberOfProcessors)
    val queue = new LinkedTransferQueue[Int]
    for (_ <- 1 to NumberOfProcessors - 1)
      queue.add(1)
   
    for (_ <- 1 to NumberOfProcessors - 1)
      exec.execute(new Runnable {
        def run(): Unit = {
          while (!Thread.interrupted()) {
            val myVal = queue.remove()
            info(s"Got val=$myVal at thread ${Thread.currentThread.getId}")                
            
            if (myVal > 50) return
          
            val mod = myVal % 2
            if (mod == 1) {
              queue add myVal + 2
            } else queue add myVal + 3
          }
        }
      })
    
//    exec.awaitTermination(60, TimeUnit.SECONDS)
    exec.shutdown()
    exec.awaitTermination(60, TimeUnit.SECONDS)
    
  }

}
