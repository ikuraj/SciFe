package scife.enumeration
package dependent

import scala.collection.mutable

class OverrideFunction[I, O, E <: Enum[O]](
  val producerFunction: (Depend[I, O], I) => E, val orgDepend: Depend[I, O])
  extends Depend[I, O] with HasLogger with Serializable {

  override type EnumType = E

  val partiallyApplied = producerFunction(this, _: I)

  def this(producerFunction: I => E, orgDepend: Depend[I, O]) =
    this( (td: Depend[I, O], i: I) => producerFunction(i), orgDepend )

  override def getEnum(parameter: I) =
    producerFunction(orgDepend, parameter)

}

// perhaps not needed
class OverrideFunctionFin[I, O](
  val producerFunction: (I => Finite[O], I) => Finite[O], val orgDepend: WrapFunctionFin[I, O])
  extends DependFinite[I, O] with HasLogger with Serializable {

  override type EnumType = Finite[O]
  
  def modifiedOrg(input: I): Finite[O] = orgDepend.producerFunction(this, input)

  val partiallyApplied = producerFunction(modifiedOrg, _: I)

  override def getEnum(parameter: I) =
    partiallyApplied(parameter)

}

class OverrideFunctionSelf[I, O, E <: Enum[O]](
  val producerFunction: (Depend[I, O], (I => E), I) => E,
  val orgDepend: WrapFunction[I, O, E])
  extends Depend[I, O] with HasLogger with Serializable {

  override type EnumType = E
  
  def transformedOrgProducer(input: I) = orgDepend.producerFunction(
    this, input
  )

  val partiallyApplied = producerFunction(this, transformedOrgProducer, _: I)

//  def this(producerFunction: I => E, orgDepend: Depend[I, O]) =
//    this( (td: Depend[I, O], i: I) => producerFunction(i), orgDepend )

  override def getEnum(parameter: I) =
    partiallyApplied(parameter)

}

//class OverrideFunctionSelf[I, O, E <: Enum[O], I2](
//  val producerFunction: (Depend[I, O], (I2 => E), I) => E,
//  val orgDepend: WrapFunction[I2, O, E],
//  val transformerFunction: I2 => I
//  )
//  extends Depend[I, O] with HasLogger with Serializable {
//
//  override type EnumType = E
//  
//  def transformedOrgProducer(input: I2) = orgDepend.producerFunction(
//    InMap(this, transformerFunction), input
//  )
//
//  val partiallyApplied = producerFunction(this, transformedOrgProducer, _: I)
//
////  def this(producerFunction: I => E, orgDepend: Depend[I, O]) =
////    this( (td: Depend[I, O], i: I) => producerFunction(i), orgDepend )
//
//  override def getEnum(parameter: I) =
//    partiallyApplied(parameter)
//
//}
