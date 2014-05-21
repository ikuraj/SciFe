package insynth.enumeration
package dependent

import insynth.util.logging._

object Concat {
  
  def apply[I, O](de1: Depend[I, O], de2: Depend[I, O]) =
    (de1, de2) match {
    	case (fe1: DependFinite[_, _], fe2: DependFinite[_, _]) =>
    	  new ConcatFinite(fe1, fe2)
    	case (ie1: DependInfinite[_, _], ie2: DependInfinite[_, _]) =>
    	  new ConcatInfinite(ie1, ie2)    	  
	  }
  
}