package insynth.enumeration
package reverse
package dependent

class WrapFunction[I, O](val producerFunction: (ReverseDepend[I, O], I) => Reverse[O])
	extends ReverseDepend[I, O] with HasLogger with Serializable {
  
  override type EnumType = Reverse[O]
  
  val partiallyApplied = producerFunction(this, _: I)
  
  def this(producerFunction: I => Reverse[O]) =
    this( (td: ReverseDepend[I, O], i: I) => producerFunction(i) )

  override def getEnum(parameter: I) =
    producerFunction(this, parameter)
  
}

class WrapFunctionFin[I, O](val producerFunction:
  (ReverseDependFinite[I, O], I) => ReverseFinite[O])
  extends ReverseDependFinite[I, O] with HasLogger with Serializable {
  
  override type EnumType = ReverseFinite[O]
  
  val partiallyApplied = producerFunction(this, _: I)
  
  def this(producerFunction: I => ReverseFinite[O]) =
    this( (td: ReverseDependFinite[I, O], i: I) => producerFunction(i) )

  override def getEnum(parameter: I) =
    producerFunction(this, parameter)
  
}