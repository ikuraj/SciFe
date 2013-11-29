package insynth
package streams
package ordered

import unordered.{ SingleStream => UnSingleStream }
import util.logging._

// NOTE this would require ordered stream
class FiniteStream[T](arr: => Vector[(T, Int)])
	extends OrderedStreamable[T] with HasLogger {
  
  var nextInd = 0
  val stream = Stream.fill( arr.size )( {
    nextInd+=1;
    fine("nextInd is " + nextInd)
    arr(nextInd-1)
  } )
  
  override def isInfinite = false
  
  override def isDepleted: Boolean = nextInd >= arr.size // wtv
  override def nextReady(ind: Int): Boolean = {
    fine("nextReady for " + ind + " is " + (ind<arr.size))
    ind < arr.size
  }
  
  override def getStream = stream map { _._1 }
  
  override def getValues = stream map { _._2 }
    
}

object FiniteStream {
  def apply[T](stream: => Vector[(T, Int)]) =
    new FiniteStream(stream)
}