package insynth.enumeration
package combinators

import Filter.FilterFunction

trait Filter[T] extends Enum[T] {

  val f: FilterFunction[T]
  
}