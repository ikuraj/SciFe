package scife.enumeration
package dependent

import scala.collection.immutable.{ Map => ScalaMap }
import scala.collection.mutable

class WrapMap[I, O, E <: Enum[O]](initMap: ScalaMap[I, E] = ScalaMap.empty)
  extends Depend[I, O] with HasLogger {
  
  override type EnumType = E
  
  var _map: mutable.Map[I, E] = mutable.Map() ++ initMap

  override def getEnum(parameter: I) =
    map(parameter)
    
  def map = _map
  
}