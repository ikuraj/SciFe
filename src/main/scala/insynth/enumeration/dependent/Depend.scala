package insynth.enumeration
package dependent

import scala.collection.immutable.Map

trait Depend[I, +O] {
  
  type EnumType <: Enum[O]

  def apply(parameter: I) = getEnum(parameter)
  
  def getEnum(parameter: I): EnumType
  
}

object Depend {
  
  def apply[I, O, E <: Enum[O]](producerFunction: (Depend[I, O], I) => E) =
    new WrapFunction(producerFunction)
  
  def apply[I, O, E <: Enum[O]](producerFunction: I => E) =
    new WrapFunction[I, O, E](producerFunction)
  
  def apply[I, O, E <: Enum[O]](producerMap: Map[I, E]) =
    new WrapMap[I, O, E](producerMap)
  
  def map[I, O, E <: Enum[O]](producerMap: Map[I, E] = Map.empty) =
    new WrapMap[I, O, E](producerMap)
  
//  def memoized[I, O](producerFunction: I => Enum[O])
//    (implicit ms: MemoizationScope = null): Depend[I, O] = {
//    val enum = new Producer(producerFunction) with Memoized[I, O]
//        
//    if (ms != null) ms add enum
//    enum
//  }
//    
//  def memoized[I, O](producerFunction: (Depend[I, O], I) => Enum[O])
//    (implicit ms: MemoizationScope = null): Depend[I, O] = {
//    val enum = new Producer(producerFunction) with Memoized[I, O]
//        
//    if (ms != null) ms add enum
//    enum
//  }
    
}