package insynth.engine

import insynth.structures.Weight._

class Properties {
  private var active = false

  def isActive = active
  def activate(){active = true}
  def deactivate(){active = false}
  
  private var minWeight: Weight = globalMinWeight
  
  def getMinWeight = minWeight
  
  def setMinWeight(weight: Weight){
   if (minWeight > weight) minWeight = weight
  }
  //TODO: We can speed up the search by not sending queries to the other 'params' if at least one of them can not fulfill the request
  //TODO: This raises the question of 'negative answer propagation'
}