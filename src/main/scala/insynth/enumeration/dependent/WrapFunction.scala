package insynth.enumeration
package dependent

import scala.collection.mutable

class WrapFunction[I, O, E <: Enum[O]](val producerFunction: (Depend[I, O], I) => E)
	extends Depend[I, O] with HasLogger with Serializable {
  
  override type EnumType = E
  
  val partiallyApplied = producerFunction(this, _: I)
  
  def this(producerFunction: I => E) =
    this( (td: Depend[I, O], i: I) => producerFunction(i) )

  override def getEnum(parameter: I) =
    producerFunction(this, parameter)
  
}

class WrapFunctionFin[I, O](val producerFunction: (DependFinite[I, O], I) => Finite[O])
  extends DependFinite[I, O] with HasLogger with Serializable {
  
  override type EnumType = Finite[O]
  
  val partiallyApplied = producerFunction(this, _: I)
  
  def this(producerFunction: I => Finite[O]) =
    this( (td: Depend[I, O], i: I) => producerFunction(i) )

  override def getEnum(parameter: I) =
    producerFunction(this, parameter)
  
}

//class WrapFunctionInf[I, O](val producerFunction: (DependInfinite[I, O], I) => E)
//  extends Depend[I, O] with HasLogger with Serializable {
//  
//  override type EnumType = E
//  
//  val partiallyApplied = producerFunction(this, _: I)
//  
//  def this(producerFunction: I => E) =
//    this( (td: Depend[I, O], i: I) => producerFunction(i) )
//
//  override def getEnum(parameter: I) =
//    producerFunction(this, parameter)
//  
//}