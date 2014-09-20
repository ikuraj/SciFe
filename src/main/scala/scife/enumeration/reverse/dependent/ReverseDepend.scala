package scife.enumeration
package reverse
package dependent

import scife.enumeration.dependent._

trait ReverseDepend[I, O] extends Depend[I, O] {

  override type EnumType = Reverse[O]

}
