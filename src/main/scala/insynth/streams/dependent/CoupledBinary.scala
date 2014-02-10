package insynth.streams
package dependent

import scala.collection.mutable

import insynth.util.logging._

case class CoupledBinary[I, O1, O2, O]
	(s1: FiniteDependent[I, O1], s2: FiniteDependent[I, O2])
	(combine: (O1, O2) => O) extends Dependent[I, O] with HasLogger {
  
  def getStream(parameter: I) = {
    val left = s1.getStream(parameter)
    val right = s2.getStream(parameter)
    
    light.Binary(left, right)(combine)    
  }
  
}