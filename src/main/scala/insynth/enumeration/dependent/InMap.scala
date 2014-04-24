package insynth.enumeration
package dependent

import scala.language.higherKinds

class InMap[I, NewIn, +O, DependIn[I, +O] <: Depend[I, O]](
  override val inner: DependIn[I, O], override val f: NewIn => I  
) extends combinators.InMap[I, NewIn, O] {
  
  override type DependType[I, +O] = DependIn[I, O]
  
}

class InMapP[I, NewIn, +O, DependIn[I, +O] <: Depend[I, O]](
  override val inner: DependIn[I, O], override val f: PartialFunction[NewIn, I]  
) extends combinators.InMap[I, NewIn, O] {
  
  override type DependType[I, +O] = DependIn[I, O]
  
}

object InMap {
  
  def apply[T, U, O](tde: Depend[T, O], modify: U => T) =
    tde match {
    	case f: DependFinite[T, O] =>
    	  new InMap(f, modify) with DependFinite[U, O]
    	case i: DependInfinite[T, O] =>
    	  new InMap(i, modify) with DependInfinite[U, O]
	  }
  
  def apply[T, U, O](tde: DependFinite[T, O], modify: U => T) =
	  new InMap(tde, modify) with DependFinite[U, O]
  
  def apply[T, U, O](tde: DependInfinite[T, O], modify: U => T) =
	  new InMap(tde, modify) with DependInfinite[U, O]
}

object InMapP {
  
  def apply[T, U, O](tde: Depend[T, O], modify: PartialFunction[U, T]) =
    tde match {
    	case f: DependFinite[T, O] =>
    	  new InMapP(f, modify) with DependFinite[U, O]
    	case i: DependInfinite[T, O] =>
    	  new InMapP(i, modify) with DependInfinite[U, O]
	  }
  
  def apply[T, U, O](tde: DependFinite[T, O], modify: PartialFunction[U, T]) =
	  new InMapP(tde, modify) with DependFinite[U, O]
  
  def apply[T, U, O](tde: DependInfinite[T, O], modify: PartialFunction[U, T]) =
	  new InMapP(tde, modify) with DependInfinite[U, O]
  
}