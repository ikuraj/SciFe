package scife.enumeration
package dependent
package combinators

trait Product[I, T, U] extends Depend[I, (T, U)] {

  val left: Depend[I, T]
  val right: Depend[I, U]

}
