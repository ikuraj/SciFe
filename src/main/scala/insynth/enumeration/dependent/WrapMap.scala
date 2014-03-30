package insynth.enumeration
package dependent

import insynth.util.logging._

import scala.collection.immutable.Map
import scala.collection.mutable

class WrapMap[I, O, E <: Enum[O]](initMap: Map[I, E] = Map.empty)
  extends Depend[I, O] with HasLogger {
  
  override type EnumType = E
  
  var _map: mutable.Map[I, E] = mutable.Map() ++ initMap

  override def getEnum(parameter: I) =
    map(parameter)
    
  def map = _map
  
}