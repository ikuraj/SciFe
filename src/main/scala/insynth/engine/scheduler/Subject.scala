package insynth.engine.scheduler

import insynth.engine.TypeAssignment

abstract class Subject extends ListenerHolder {

  def notifyListeners(tas:List[TypeAssignment]){
    listeners.foreach(_.notify(tas))
  }
  
}


//Still all listeners must register before the synthesis starts 
abstract class ListenerHolder {
  
  protected var listeners = Set.empty[Listener]
  
  def registerAllListener(listeners:Set[Listener]) { this.listeners ++= listeners}
  
  def registerListener(listener:Listener) { listeners += listener}
  
  def unregisterListener(listener:Listener) {listeners -= listener}
  
}