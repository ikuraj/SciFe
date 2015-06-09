package scife
package enumeration
package lazytraversal

import scife.util._

trait NoSkip[+A] extends Enum[A] with Skippable[A] {
  
  override def next(i: Int) = i + 1

}