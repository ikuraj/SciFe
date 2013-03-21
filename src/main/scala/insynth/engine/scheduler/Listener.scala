package insynth.engine.scheduler

import insynth.engine.TypeAssignment

trait Listener {
  
  //notifies listener when TAs are updated
  def notify(tas:List[TypeAssignment]){
    tas.foreach(notify)
  }
  
  //notifies listener when TA is updated
  def notify(ta:TypeAssignment)
}