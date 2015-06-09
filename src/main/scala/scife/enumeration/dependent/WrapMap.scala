package scife.enumeration
package dependent

import scala.collection.immutable.{ Map => ScalaMap }
import scala.collection.mutable

import scife.util._

import scala.language.higherKinds

class WrapMap[I, O, E[A] <: Enum[A]](initMap: ScalaMap[I, E[O]] = ScalaMap.empty)
  extends Depend[I, O] with HasLogger {

  override type EnumSort[A] = E[A]

  var _map: mutable.Map[I, E[O]] = mutable.Map() ++ initMap

  override def getEnum(parameter: I) =
    map(parameter)

  def map = _map

}
