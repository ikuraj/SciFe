package insynth.enumeration
package dependent

import scala.collection.mutable

import insynth.util.logging._

class WrapFunction[I, O, E <: Enum[O]](val producerFunction: (Depend[I, O], I) => E)
	extends Depend[I, O] with HasLogger with Serializable {
  
  override type EnumType = E
  
  val partiallyApplied = producerFunction(this, _: I)
  
  def this(producerFunction: I => E) =
    this( (td: Depend[I, O], i: I) => producerFunction(i) )

  override def getEnum(parameter: I) =
    producerFunction(this, parameter)
  
}