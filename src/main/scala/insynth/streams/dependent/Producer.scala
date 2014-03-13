package insynth
package streams
package dependent

import scala.collection.mutable

import light._
import util.logging._

class Producer[I, O](val producerFunction: (Dependent[I, O], I) => Enumerable[O])
	extends Dependent[I, O] with HasLogger with Serializable {
  
  val partiallyApplied = producerFunction(this, _: I)
  
  def this(producerFunction: I => Enumerable[O]) =
    this( (td: Dependent[I, O], i: I) => producerFunction(i) )

  override def getStream(parameter: I) =
    producerFunction(this, parameter)
  
}

class MapProducer[I, O](initMap: Map[I, Enumerable[O]] = Map.empty)
  extends Dependent[I, O] with HasLogger {
  
  var _map: mutable.Map[I, Enumerable[O]] = mutable.Map() ++ initMap

  override def getStream(parameter: I) =
    map(parameter)
    
  def map = _map
  
}

object Producer {
  
  def apply[I, O](producerFunction: I => Enumerable[O]) =
    new Producer(producerFunction)
  
  def apply[I, O](producerMap: Map[I, Enumerable[O]]) =
    new Producer(producerMap)
  
  def map[I, O](producerMap: Map[I, Enumerable[O]] = Map.empty) =
    new Producer(producerMap)
  
  def memoized[I, O](producerFunction: I => Enumerable[O]): Dependent[I, O] =
    new Producer(producerFunction) with Memoized[I, O]
    
  def memoized[I, O](producerFunction: (Dependent[I, O], I) => Enumerable[O]): Dependent[I, O] =
    new Producer(producerFunction) with Memoized[I, O]
    
}