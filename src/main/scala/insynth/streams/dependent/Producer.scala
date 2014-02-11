package insynth
package streams
package dependent

import scala.collection.mutable

import light._
import util.logging._

class Producer[I, O](var producerFunction: I => Enumerable[O])
	extends Dependent[I, O] with HasLogger {

  override def getStream(parameter: I) =
    producerFunction(parameter)
  
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
  
  def memoized[I, O](producerFunction: I => Enumerable[O]) =
    new Producer(producerFunction) with Memoized[I, O]
    
}