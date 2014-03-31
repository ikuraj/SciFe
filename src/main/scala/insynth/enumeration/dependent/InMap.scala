package insynth.enumeration
package dependent

class InMap[I, NewIn, +O](
  override val inner: Depend[I, O], override val f: NewIn => I  
) extends combinators.InMap[I, NewIn, O] {
  
}

object Map {
  
  def apply[T, U, O](tde: Depend[T, O], modify: U => T) =
    tde match {
    	case f: DependFinite[T, O] =>
    	  new InMap(f, modify)
    	case i: DependInfinite[T, O] =>
    	  new InMap(i, modify)
	  }
  
  def apply[T, U, O](tde: DependFinite[T, O], modify: U => T) =
	  new InMap(tde, modify)
  
  def apply[T, U, O](tde: DependInfinite[T, O], modify: U => T) =
	  new InMap(tde, modify)
  
}