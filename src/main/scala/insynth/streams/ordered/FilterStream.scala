package insynth.streams.ordered

import insynth.streams._
import insynth.streams.unordered.{ UnaryStream => UnUnaryStream }

import scala.annotation.tailrec

class FilterStream[T](val streamable: OrderedStreamable[T], filterFun: T => Boolean)
	extends OrderedStreamable[T] {
  
  var numberOfEnumerated = 0
  var numberOfFiltered = 0
  
  override def isInfinite = streamable.isInfinite
  
  override def isDepleted: Boolean = streamable.isDepleted
  override def nextReady(ind: Int): Boolean =
    if (ind < numberOfEnumerated) true
    else { //streamable.nextReady(ind + numberOfFiltered)
      info("nextReady(%d)".format(ind) + "=" + valIterator.hasNext)
      valIterator.hasNext
    }
    
  lazy val valIterator = streamable.getValues.iterator.buffered
  lazy val elIterator = streamable.getStream.iterator.buffered  
    
  def loop: Stream[(T, Int)] = {
    while (valIterator.hasNext) {
      val currEl = elIterator.head
      val isFine = filterFun(currEl)
      finest("Evaluation of " + currEl + " resulted in " + isFine)      
      
      if (isFine) {
        val res = (currEl, valIterator.head) #:: {
          elIterator.next
          valIterator.next          
          loop
        }
        numberOfEnumerated += 1
        return res
      }
      
      numberOfFiltered += 1
      elIterator.next
      valIterator.next
    }
    Stream.empty
  }
      
  lazy val stream = loop
  
  override def getStream = stream map (_._1)
  
  override def getValues = stream map (_._2)
  
}

object FilterStream {
  def apply[T, U](streamable: OrderedStreamable[T], filterFun: T => Boolean) =
    new FilterStream(streamable, filterFun)
}