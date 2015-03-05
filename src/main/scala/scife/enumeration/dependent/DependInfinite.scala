package scife.enumeration
package dependent

import scala.language.higherKinds

trait DependInfinite[I, O] extends Depend[I, O] {

  override type EnumSort[A] <: Infinite[A]

}
