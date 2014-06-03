package insynth.enumeration.reverse
package dependent

import insynth.{ enumeration => e }
import e.dependent.combinators

import scala.language.higherKinds

class InMap[I, NewIn, O](
  override val inner: ReverseDepend[I, O], override val f: NewIn => I  
) extends combinators.InMap[I, NewIn, O] {
  
  override type DependType = ReverseDepend[I, O]
  
}

//object InMap {
//  
//  def apply[T, U, O](tde: DependReverse[T, O], modify: U => T) =
//	  new e.dependent.InMap(tde, modify) with DependReverse[U, O]
//  
//}