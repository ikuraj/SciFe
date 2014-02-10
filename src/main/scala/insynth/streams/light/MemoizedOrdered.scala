package insynth.streams
package light

import scala.collection.mutable._

import insynth.util.logging.HasLogger

trait Memoized[T] extends Enumerable[T] with HasLogger {
  
  val memoizedValues = new ArrayBuffer[T]()
  
  override abstract def apply(ind: Int) =
    if (ind < memoizedValues.size)
      memoizedValues(ind)
    else {
      val nextValue = super.apply(ind)
      memoizedValues += nextValue
      nextValue
    }      
  
}