//package insynth.streams
//package dependent
//
//import scala.collection.mutable
//
//import insynth.util.logging._
//
//// NOTE this only works if all dependent streams are finite
//case class FiniteBinary[I, O1, I2, O2, O]
//	(s1: FiniteDependentStreamable[I, O1], s2: FiniteDependentStreamable[I2, O2])
//	(chain: O1 => I2) (combine: (O1, O2) => O) extends DependentStreamable[I, O] with HasLogger {
//  
//  def getStream(parameter: I) =
//    forLeftStream(s1.getStream(parameter))
//    
//  def forLeftStream(s: Stream[O1]): Stream[O] = 
//    if (s.nonEmpty)
//    	forLeftValue(s.head) #::: forLeftStream(s.tail)
//  	else Stream.empty
//    
//  def forLeftValue(v: O1) =
//    s2.getStream(chain(v)) map { combine(v, _) }
//  
//}