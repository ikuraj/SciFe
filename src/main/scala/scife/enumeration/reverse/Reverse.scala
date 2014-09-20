package scife.enumeration
package reverse

// NOTE could explore solution with Invariant Reverse and an implicit
// conversion to its objects

trait Reverse[A] extends Enum[A] {

  def reverse(a: A): Int

}

trait ReverseFinite[A] extends Finite[A] with Reverse[A]

trait ReverseInfinite[A] extends Infinite[A] with Reverse[A]