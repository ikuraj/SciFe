package insynth.enumeration
package combinators

abstract class Map[T, U](e: Enum[T], f: T => U) extends Enum[U] {
  
  override def apply(ind: Int) =
    f( e.apply(ind) )

}