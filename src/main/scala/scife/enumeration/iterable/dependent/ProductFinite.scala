package scife
package enumeration
package iterable
package dependent

import scife.{ enumeration => e }
import enumeration.{ lzy => elzy }
import scife.enumeration.{ dependent => d }

import scife.util.logging._

import scala.language.higherKinds

case class ProductFinite[I, O1, O2]
  (s1: d.DependFinite[I, O1] { type EnumSort[A] = ResetIterFinite[A] },
    s2: d.DependFinite[I, O2] { type EnumSort[A] = ResetIterFinite[A] })
  extends d.DependFinite[I, (O1, O2)] {

  override type EnumSort[A] = ResetIterFinite[A]

  def getEnum(parameter: I) = {
    val e1 = s1.getEnum(parameter)
    val e2 = s2.getEnum(parameter)

    Product( e1, e2 )
  }

}
