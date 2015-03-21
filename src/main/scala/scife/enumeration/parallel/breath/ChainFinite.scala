package scife.enumeration
package parallel
package breadth

import combinators._
import scife.{ enumeration => e }
import e.dependent._

import scife.util._

import scala.collection.JavaConversions._

class ChainFiniteCombine[I, O, R]
  (s1: Finite[I], val s2: DependFinite[I, O], combine: (I, O) => R)
  extends Finite[R] with HasLogger {
  
  @volatile
  var built = false
    //new java.util.concurrent.atomic.AtomicBoolean(false)
  val covered = new java.util.concurrent.atomic.AtomicInteger(0)
  val finished = new java.util.concurrent.atomic.AtomicInteger(0)
  
  val innerEnumQueue = new java.util.concurrent.LinkedBlockingQueue[Finite[R]]

  var rr: Finite[R] = _

  def build = {
//    val streams =
//      for (ind <- 0 until s1.size; leftProduced = s1(ind); rightStream = s2.getEnum( leftProduced );
//        if rightStream.size > 0 ) yield {
//          e.Map( rightStream, { (rightProduced: O) => combine(leftProduced, rightProduced) })
//        }

    rr = e.lzy.breadth.ConcatFinite[R]( innerEnumQueue.toList )
//    built.set(true)
    built = true
  }
  
  def uncover(leftInd: Int) = {
    val leftProduced = s1(leftInd)
    val rightEnum = s2.getEnum( leftProduced )
    if (rightEnum.size > 0) {
      val newEnum = e.Map( rightEnum, { (rightProduced: O) => combine(leftProduced, rightProduced) })
      innerEnumQueue add newEnum
    }
  }
  
  def init: Unit = {
//    assert(s1.size > 0)
    if (covered.get < s1.size) {
      var nowCovering: Int = covered.getAndIncrement
      var myFinished: Int = -1
      while (nowCovering < s1.size) {
//        println(s"${this.hashCode()}:uncovering/all=$nowCovering/${s1.size}")
        uncover(nowCovering)
        myFinished = finished.incrementAndGet()
        nowCovering = covered.getAndIncrement
      }
      if (myFinished == s1.size) {
//        println("${this.hashCode()}:building!")
        build
        this.synchronized { this.notifyAll }
        return
      }
    }
    waitForBuild
  }
  
  def waitForBuild {
    if (!built) {
      this.synchronized {
        if (!built) {
//          println(s"${this.hashCode()}:sleeping")
          this.wait()
        }
      }
    }
  }

  override def size = {
    init
    rr.size
  }

  override def apply(ind: Int) = {
    init  
    rr(ind)
  }

}