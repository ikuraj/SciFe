package scife
package enumeration
package iterable
package lzy
package dependent

import scife.{ enumeration => e }
import e.{ dependent => d }
import enumeration.{ lzy => elzy }

import scalaz.LazyTuple2

import scife.util.logging._

import scala.language.higherKinds

case class ProductFinite[I, O1, O2]
  (s1: LazyDependFinite[I, O1], s2: LazyDependFinite[I, O2])
  extends d.DependFinite[I, LazyTuple2[O1, O2]] {

  override type EnumSort[A] = LazyEnumFinite[A]

  def getEnum(parameter: I) =
    Product.touchable( s1.getEnum(parameter), s2.getEnum(parameter) )

}
