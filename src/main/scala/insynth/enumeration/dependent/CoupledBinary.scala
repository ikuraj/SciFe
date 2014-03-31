//package insynth.streams
//package dependent
//
//import scala.collection.mutable
//
//import insynth.util.logging._
//
//case class CoupledBinary[I, O1, O2]
//	(s1: Depend[I, O1], s2: Depend[I, O2])
//	extends Depend[I, (O1, O2)] with HasLogger {
//  
//  def getStream(parameter: I) = {
//    val left = s1.getStream(parameter)
//    val right = s2.getStream(parameter)
//    
//    light.Binary(left, right)    
//  }
//  
//}
//
//case class CoupledBinaryCombine[I, O1, O2, O]
//	(s1: Depend[I, O1], s2: Depend[I, O2])
//	(combine: (O1, O2) => O) extends Depend[I, O] with HasLogger {
//  
//  def getStream(parameter: I) = {
//    val left = s1.getStream(parameter)
//    val right = s2.getStream(parameter)
//    
//    light.Binary(left, right, combine)    
//  }
//  
//}
//
//case class BinaryPairs[I1, O1, I2, O2]
//	(s1: Depend[I1, O1], s2: Depend[I2, O2])
//	extends Depend[(I1, I2), (O1, O2)] with HasLogger {
//  
//  def getStream(parameter: (I1, I2)) = {
//    val (i1, i2) = parameter
//    val left = s1.getStream(i1)
//    val right = s2.getStream(i2)
//    
//    light.Binary(left, right)    
//  }
//  
//}