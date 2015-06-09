package scife.enumeration
package dependent

import scala.collection.mutable

import scife.util._

import scala.language.higherKinds

class WrapFunction[I, O, E[A] <: Enum[A]](val producerFunction: (Depend[I, O], I) => E[O])
  extends Depend[I, O] with HasLogger with Serializable {

  override type EnumSort[A] = E[A]

  val partiallyApplied = producerFunction(this, _: I)

  def this(producerFunction: I => E[O]) =
    this( (td: Depend[I, O], i: I) => producerFunction(i) )

  override def getEnum(parameter: I) =
    producerFunction(this, parameter)

}

class WrapFunctionFin[I, O](val producerFunction: (DependFinite[I, O], I) => Finite[O])
  extends DependFinite[I, O] with HasLogger with Serializable {

  override type EnumSort[A] = Finite[A]

  val partiallyApplied = producerFunction(this, _: I)

  def this(producerFunction: I => Finite[O]) =
    this( (td: Depend[I, O], i: I) => producerFunction(i) )

  override def getEnum(parameter: I) =
    producerFunction(this, parameter)

}

class WrapFunctionP[I, O, E[A] <: Enum[A]](val producerFunction: PartialFunction[(Depend[I, O], I), E[O]])
  extends Depend[I, O] with HasLogger with Serializable {

  override type EnumSort[A] = E[A]

  val partiallyApplied = producerFunction(this, _: I)

//  def this(producerFunction: PartialFunction[I, E]) =
//    this( { case (td: Depend[I, O], i: I) => producerFunction(i) } )

  override def getEnum(parameter: I) =
    producerFunction(this, parameter)
}

class WrapFunctionFinP[I, O, E[A] <: Finite[A]]
  (val producerFunction: PartialFunction[(DependFinite[I, O], I), E[O]])
  extends DependFinite[I, O] with HasLogger with Serializable {

  override type EnumSort[A] = E[A]

  val partiallyApplied = producerFunction(this, _: I)

//  def this(producerFunction: PartialFunction[I, E]) =
//    this(
//      { case (td: DependFinite[I, O], i: I) => producerFunction(i) }:
//      PartialFunction[(DependFinite[I, O], I), E]
//    )

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

class WrapFunctionTest[I, O, E[A] <: Enum[A]](val producerFunction:
  (Depend[I, O] { type EnumSort[A] = E[A] }, I) => E[O])
  extends Depend[I, O] with HasLogger with Serializable {

  override type EnumSort[A] = E[A]

  val partiallyApplied = producerFunction(this, _: I)

  override def getEnum(parameter: I) =
    producerFunction(this, parameter)

}

class WrapFunctionTest2[I, O, E[A] <: Enum[A]](val producerFunction:
  (Depend[I, O] { type EnumSort[A] = E[A] }, I) => E[O])
  extends Depend[I, O] with HasLogger with Serializable {

  override type EnumSort[A] = E[A]

  override def getEnum(parameter: I) =
    producerFunction(this, parameter)

}

//class WrapFunctionTest2[I, O, E[A] <: Enum[A],
//  D[I, O] <: Depend[I, O]{ type EnumSort[A] = E[A] }](val producerFunction:
//  (D[I, O], I) => E[O])
//  extends Depend[I, O] with HasLogger with Serializable {
//
//  override type EnumSort[A] = E[A]
//
//  val partiallyApplied = producerFunction(this, _: I)
//
//  override def getEnum(parameter: I) =
//    producerFunction(this, parameter)
//
//}