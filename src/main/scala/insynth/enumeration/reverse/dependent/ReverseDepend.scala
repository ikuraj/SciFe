package insynth.enumeration
package reverse
package dependent

import insynth.enumeration.dependent._
import insynth.enumeration.{ dependent => d }

import scala.reflect._
import scala.language.higherKinds

trait ReverseDepend[I, O] extends Depend[I, O] {
  
  override type EnumType = Reverse[O]

}

trait ReverseDependFinite[I, O] extends DependFinite[I, O] {
  
  override type EnumType = ReverseFinite[O]

}

trait ReverseDependInfinite[I, O] extends DependInfinite[I, O] {
  
  override type EnumType = ReverseInfinite[O]

}

object ReverseDepend {
    
//  def apply[I, O](producerFunction: (ReverseDepend[I, O], I) => Reverse[O]): ReverseDepend[I, O] = {
//    val reverseTag = implicitly[ClassTag[Reverse[_]]]
//
//    implicitly[ClassTag[F[_]]] match {
//      case `reverseTag` =>
//        val fun = producerFunction.asInstanceOf[(d.Depend[I, O], I) => Reverse[O]]
//        new d.WrapFunction[I, O, Reverse[O]](fun) with DependReverse[I, O] with
//          d.DependFinite[I, O] with m.dependent.Memoized[I, O]
//          
//    }
//  }
  
  def apply[I, O](fun: I => Reverse[O]): ReverseDepend[I, O] = {
    new WrapFunction[I, O](fun) with ReverseDepend[I, O] with Depend[I, O]
  }
  
//  def apply[I, O, E <: Reverse[O]](fun: (d.Depend[I, O], I) => E): DependReverse[I, O] = {
//    new d.WrapFunction(fun) with DependReverse[I, O]
//  }
//  
//  def apply[I, O, F[O] <: Enum[O]](producerFunction: I => F[O])
//    (implicit ct: ClassTag[F[_]]): DependReverse[I, O] = {
//    val reverseTag = implicitly[ClassTag[Reverse[_]]]
//
//    implicitly[ClassTag[F[_]]] match {
//      case `reverseTag` =>
//        val fun = producerFunction.asInstanceOf[I => Reverse[O]]
//        new d.WrapFunction[I, O, Reverse[O]](fun) with DependReverse[I, O] with d.DependFinite[I, O]
////      case _: Infinite[_] =>
////        val fun = producerFunction.asInstanceOf[I => Infinite[O]]
////        new d.WrapFunction[I, O, Infinite[O]](fun) with DependReverse[I, O] with d.DependInfinite[I, O]
////      case _ =>
////        new d.WrapFunction[I, O, F[O]](producerFunction) with DependReverse[I, O]
//    }
//  }
    
//  def memoized[I, O, F[O] <: Enum[O]](producerFunction: (d.Depend[I, O], I) => F[O])
//    (implicit ct: ClassTag[F[_]], ms: m.MemoizationScope = null): DependReverse[I, O] = {
//    val reverseTag = implicitly[ClassTag[Reverse[_]]]
//    val enum =
//      implicitly[ClassTag[F[_]]] match {
//        case `reverseTag` =>
//          val fun = producerFunction.asInstanceOf[(d.Depend[I, O], I) => Reverse[O]]
//          new d.WrapFunction[I, O, Reverse[O]](fun) with DependReverse[I, O] with
//            d.DependFinite[I, O] with m.dependent.Memoized[I, O]
//            
//      }
//        
//    if (ms != null) ms add enum
//    enum
//  }

//  def apply[T](enum: Enum[T], v: T) = {
//    enum
//  }
//  
//  def apply[T](enum: Finite[T], v: T) = enum match {
//    case e@Empty =>
//      e
//    case m: Map
//  override def reverse[V >: U](a: V) =
//    en.Map( enum.reverse( revFun( a.asInstanceOf[U] ) ), modify)
//  }

}