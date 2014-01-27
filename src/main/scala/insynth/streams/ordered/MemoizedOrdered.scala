package insynth.streams
package ordered

import scala.collection._

import insynth.util.logging.HasLogger

trait Memoized[T] extends IntegerWeightStreamable[T] with HasLogger {
  
  // will memoize the stream (without forcing the computation)
  lazy val memoizedStream = super.getValuedStream
  
  override abstract def getValuedStream = memoizedStream
  
}

//trait Memoized[T] extends IntegerWeightStreamable[T] with HasLogger {
//  
//  // will memoize the stream (without forcing the computation)
//  var allElements = mutable.MutableList[(T, Int)]()
//  var size = 0
//  lazy val iterator = super.getValuedStream.iterator
//  
//  def loop(ind: Int) = {
//    if (ind < size) 
//  }
//  
//  override abstract def getValuedStream = memoizedStream
//  
//}