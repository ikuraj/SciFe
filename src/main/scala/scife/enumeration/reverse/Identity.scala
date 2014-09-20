package scife.enumeration
package reverse

import scife.{ enumeration => e }

trait Identity extends ReverseInfinite[Int] {

  override def reverse(ind: Int) = {
    ind
  }

}

class WrapRange(range: Range) extends e.WrapRange(range) with ReverseFinite[Int] {

  override def reverse(el: Int) = {
    el - range.start
  }

}