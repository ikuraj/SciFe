package insynth.streams
package light

import java.util.ArrayList
import scala.collection.mutable._

import insynth.util.logging.HasLogger

trait Memoized[T] extends Enumerable[T] with Memoizable with HasLogger {
  
  val memoizedFlags = new BitSet()
  val memoizedValues = new ArrayBuffer[T]()
//  val memoizedValues = new ArrayList[T](128)
  
  override abstract def apply(ind: Int) =
    if (memoizedFlags contains ind)
      memoizedValues(ind)
    else {
      val nextValue = super.apply(ind)
      memoizedFlags += ind
      memoizedValues.appendAll( List.fill(ind - memoizedValues.size + 1)(nextValue) )
//      memoizedValues(ind) = nextValue
      nextValue
    }
      
  override def clearMemoization {
    memoizedFlags.clear
    memoizedValues.clear
  }
  
}