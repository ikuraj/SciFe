package insynth.engine.scheduler

import insynth.engine.TypeAssignment
import insynth.engine.Environment

import scala.collection.mutable.Queue
import scala.collection.mutable.PriorityQueue

trait Scheduler extends Listener {
  
  def hasFinished():Boolean
  
  def add(ta:TypeAssignment)
  
  def next():TypeAssignment
  
}

class BFSScheduler extends Scheduler {
  
  private var queue = Queue.empty[TypeAssignment]
  
  def hasFinished():Boolean = queue.isEmpty
  
  def add(ta:TypeAssignment){
    val active = ta.getProperties.isActive
    if (!active) {
      ta.getProperties.activate()
      queue.enqueue(ta)
    }
  }
  
  def next():TypeAssignment = {
    val ta = queue.dequeue()
    ta.getProperties.deactivate()
    ta
  }
  
  def notify(ta:TypeAssignment){
    val properties = ta.getProperties
    if (!properties.isActive) add(ta)
  }
}

class WeightScheduler extends Scheduler {

  private var pq = new PriorityQueue[TypeAssignment]()(new Weighting())
  
  def hasFinished():Boolean = pq.isEmpty
  
  def add(ta:TypeAssignment){
    val active = ta.getProperties.isActive
    if (!active) {
      ta.getProperties.activate()
      pq.enqueue(ta)
    }
  }
  
  def next():TypeAssignment = {
    val ta = pq.dequeue()
    ta.getProperties.deactivate()
    ta
  }
  
  def notify(ta:TypeAssignment){
    val properties = ta.getProperties
    if (!properties.isActive) add(ta)
  }
  
  class Weighting[T <: TypeAssignment] extends Ordering[T] {
    def compare(x: T, y: T): Int = y.getProperties.getMinWeight.compare(x.getProperties.getMinWeight)
  }  
}