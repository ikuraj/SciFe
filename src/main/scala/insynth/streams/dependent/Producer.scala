package insynth
package streams
package dependent

import light._
import util.logging._

class Producer[I, O](var producerFunction: I => Enumerable[O])
	extends Dependent[I, O] with HasLogger {

  override def getStream(parameter: I) =
    producerFunction(parameter)
  
}

object Producer {
  
  def apply[I, O](producerFunction: I => Infinite[O]) =
    new Producer(producerFunction)
  
  def finite[I, O](producerFunction: I => Finite[O]) =
    new Producer(producerFunction) with FiniteDependent[I, O]
    
}