package insynth
package streams
package dependent

import util.logging._

class Producer[I, O](var producerFunction: I => Stream[O])
	extends DependentStreamable[I, O] with HasLogger {

  override def getStream(parameter: I) =
    producerFunction(parameter)
  
}

object Producer {
  
  def apply[I, O](producerFunction: I => Stream[O]) =
    new Producer(producerFunction)
  
  def finite[I, O](producerFunction: I => Stream[O]) =
    new Producer(producerFunction) with FiniteDependentStreamable[I, O]
    
}