package scife
package enumeration
package lazytraversal

import scife.util._

trait Skippable[+A] extends Enum[A] with HasLogger {
  
//  self: Enum[A] =>
    
  def next(i: Int): Int

}