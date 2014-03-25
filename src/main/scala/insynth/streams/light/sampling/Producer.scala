package insynth
package streams
package light
package sampling

import streams.{ dependent => dep }

import util.logging._

import scala.collection.mutable

class Producer[I, O](val producerFunction: (dep.Dependent[I, O], I) => SamplableEnum[O])
	extends Dependent[I, O] {

  def this(producerFunction: I => SamplableEnum[O]) =
    this( (td: dep.Dependent[I, O], i: I) => producerFunction(i) )

  override def getStream(parameter: I): SamplableEnum[O] =
    producerFunction(this, parameter)
  
}

// because we use mutable.Map
trait Memoized[I, O] extends Dependent[I, O] with Memoizable {

  val memoizedMap = mutable.Map[I, SamplableEnum[O]]()
  
  override abstract def getStream(parameter: I) = {
    memoizedMap.getOrElseUpdate(parameter, super.getStream(parameter))
  }
  
  override def clearMemoization = memoizedMap.clear
  
}

object Producer {
  
  def apply[I, O](producerFunction: I => SamplableEnum[O]) =
    new Producer(producerFunction)
  
  def apply[I, O](producerMap: Map[I, SamplableEnum[O]]) =
    new Producer(producerMap)
  
  def map[I, O](producerMap: Map[I, SamplableEnum[O]] = Map.empty) =
    new Producer(producerMap)
  
  def memoized[I, O](producerFunction: I => SamplableEnum[O])
    (implicit ms: MemoizationScope = null): Dependent[I, O] = {
    val enum = new Producer(producerFunction) with Memoized[I, O]
        
    if (ms != null) ms add enum
    enum
  }
    
  def memoized[I, O](producerFunction: (dep.Dependent[I, O], I) => SamplableEnum[O])
    (implicit ms: MemoizationScope = null): Dependent[I, O] = {
    val enum = new Producer(producerFunction) with Memoized[I, O]
        
    if (ms != null) ms add enum
    enum
  }
    
}