package insynth
package streams
package ordered

import unordered.{ SingleStream => UnSingleStream }
import util.logging._

// NOTE this would require ordered stream
class FiniteStream[T](arr: => Seq[(T, Int)])
	extends OrderedStreamable[T] with HasLogger {
  
  var nextInd = 0
  val iterator = arr.iterator
  val stream = Stream.fill( arr.size )( {
    nextInd+=1;
    fine("nextInd is " + nextInd)
    iterator.next
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
  def apply[T](stream: => Seq[(T, Int)]) =
    new FiniteStream(stream)
}