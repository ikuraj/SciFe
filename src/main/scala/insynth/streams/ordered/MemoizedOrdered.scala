package insynth.streams
package ordered

import insynth.util.logging.HasLogger

trait Memoized[T] extends IntegerWeightStreamable[T] with HasLogger {
  
  // will memoize the stream (without forcing the computation)
  lazy val memoizedStream = super.getValuedStream
  
  override abstract def getValuedStream = memoizedStream
  
}