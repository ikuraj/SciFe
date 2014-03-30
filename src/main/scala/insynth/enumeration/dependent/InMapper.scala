package insynth.enumeration
package dependent

class InMap[I, NewIn, +O](
  override val inner: Depend[I, O], override val f: I => NewIn  
) extends combinators.InMap[I, NewIn, O] {
  
}

object Map {
  
  def apply[T, U, O](tde: Depend[T, O], modify: T=>U) =
    tde match {
    	case f: DependFinite[_, _] =>
    	  new InMap(f, modify) with DependFinite[U, O]
    	case i: DependInfinite[_, _] =>
    	  new InMap(i, modify) with DependInfinite[U, O]
	  }
  
  def apply[T, U, O](tde: DependFinite[T, O], modify: T=>U) =
	  new InMap(tde, modify) with DependFinite[U, O]
  
  def apply[T, U, O](tde: DependInfinite[T, O], modify: T=>U) =
	  new InMap(tde, modify) with DependInfinite[U, O]
  
}