package scife.enumeration
package reverse
package dependent

import scife.{ enumeration => e }

import scala.language.higherKinds

object InMap {

  def apply[T, U, O](tde: DependReverse[T, O], modify: U => T) =
    new e.dependent.InMap(tde, modify) with DependReverse[U, O]

}
