package insynth.streams
package dependent

import scala.collection.mutable

import insynth.util.logging._

case class CoupledBinary[I, O1, O2]
	(s1: Dependent[I, O1], s2: Dependent[I, O2])
	extends Dependent[I, (O1, O2)] with HasLogger {
  
  def getStream(parameter: I) = {
    val left = s1.getStream(parameter)
    val right = s2.getStream(parameter)
    
    light.Binary(left, right)    
  }
  
}

case class CoupledBinaryCombine[I, O1, O2, O]
	(s1: Dependent[I, O1], s2: Dependent[I, O2])
	(combine: (O1, O2) => O) extends Dependent[I, O] with HasLogger {
  
  def getStream(parameter: I) = {
    val left = s1.getStream(parameter)
    val right = s2.getStream(parameter)
    
    light.Binary(left, right, combine)    
  }
  
}

case class BinaryPairs[I1, O1, I2, O2]
	(s1: Dependent[I1, O1], s2: Dependent[I2, O2])
	extends Dependent[(I1, I2), (O1, O2)] with HasLogger {
  
  def getStream(parameter: (I1, I2)) = {
    val (i1, i2) = parameter
    val left = s1.getStream(i1)
    val right = s2.getStream(i2)
    
    light.Binary(left, right)    
  }
  
}