package insynth.enumeration
package dependent

import scala.collection.immutable.{ Map => ScalaMap }
import scala.reflect._
import scala.language.higherKinds

trait Depend[I, +O] {
  
  type EnumType <: Enum[O]

  def apply(parameter: I) = getEnum(parameter)
  
  def getEnum(parameter: I): EnumType
  
}

object Depend {
  
  def apply[I, O, E <: Enum[O]](producerFunction: (Depend[I, O], I) => E) =
    new WrapFunction(producerFunction)
  
  def rec[I, O, E <: Enum[O]](producerFunction: (Depend[I, O], I) => E) =
    new WrapFunction(producerFunction)
  
  def apply[I, O, F[O] <: Enum[O]](producerFunction: I => F[O])
  	(implicit ct: ClassTag[F[_]]): Depend[I, O] = {
    val finiteTag = implicitly[ClassTag[Finite[_]]]
    val infiniteTag = implicitly[ClassTag[Infinite[_]]]
    implicitly[ClassTag[F[_]]] match {
      case `finiteTag` =>
        val fun = producerFunction.asInstanceOf[I => Finite[O]]
      	new WrapFunction[I, O, Finite[O]](fun) with DependFinite[I, O]
      case _: Infinite[_] =>
        val fun = producerFunction.asInstanceOf[I => Infinite[O]]
      	new WrapFunction[I, O, Infinite[O]](fun) with DependInfinite[I, O]
      case _ =>
        new WrapFunction[I, O, F[O]](producerFunction)
    }
  }
  
//  def apply[I, O](producerFunction: I => Finite[O]) =
//    new WrapFunction[I, O, Finite[O]](producerFunction) with DependFinite[I, O]
//  
//  def apply[I, O](producerFunction: I => Infinite[O]) =
//    new WrapFunction[I, O, Infinite[O]](producerFunction) with DependInfinite[I, O]
//  
//  def apply[I, O, E <: Enum[O]](producerMap: ScalaMap[I, E]) =
//    new WrapMap[I, O, E](producerMap)
  
  def map[I, O, E <: Enum[O]](producerMap: ScalaMap[I, E] = ScalaMap.empty) =
    new WrapMap[I, O, E](producerMap)
  
  import memoization.MemoizationScope
  import memoization.dependent._
    
  def memoized[I, O, F[O] <: Enum[O]](producerFunction: I => F[O])
    (implicit ct: ClassTag[F[_]], ms: MemoizationScope = null): Depend[I, O] = {
    val finiteTag = implicitly[ClassTag[Finite[_]]]
    val infiniteTag = implicitly[ClassTag[Infinite[_]]]
    val enum =
      implicitly[ClassTag[F[_]]] match {
        case `finiteTag` =>
          val fun = producerFunction.asInstanceOf[I => Finite[O]]
          new WrapFunction[I, O, Finite[O]](fun) with DependFinite[I, O] with Memoized[I, O]
        case _: Infinite[_] =>
          val fun = producerFunction.asInstanceOf[I => Infinite[O]]
          new WrapFunction[I, O, Infinite[O]](fun) with DependInfinite[I, O] with Memoized[I, O]
        case _ =>
          new WrapFunction[I, O, F[O]](producerFunction) with Memoized[I, O]
      }
        
    if (ms != null) ms add enum
    enum
  }
    
  def memoized[I, O, F[O] <: Enum[O]](producerFunction: (Depend[I, O], I) => F[O])
    (implicit ct: ClassTag[F[_]], ms: MemoizationScope = null): Depend[I, O] = {
    val finiteTag = implicitly[ClassTag[Finite[_]]]
    val infiniteTag = implicitly[ClassTag[Infinite[_]]]
    val enum =
      implicitly[ClassTag[F[_]]] match {
        case `finiteTag` =>
          val fun = producerFunction.asInstanceOf[(Depend[I, O], I) => Finite[O]]
          new WrapFunction[I, O, Finite[O]](fun) with DependFinite[I, O] with Memoized[I, O]
        case _: Infinite[_] =>
          val fun = producerFunction.asInstanceOf[(Depend[I, O], I) => Infinite[O]]
          new WrapFunction[I, O, Infinite[O]](fun) with DependInfinite[I, O] with Memoized[I, O]
        case _ =>
          new WrapFunction[I, O, F[O]](producerFunction) with Memoized[I, O]
      }
        
    if (ms != null) ms add enum
    enum
  }
    
}