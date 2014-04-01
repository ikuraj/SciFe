//package insynth
//package enumeration
//package reverse
//
//import insynth.{ enumeration => e }
//
///**
// * Wrapper around the Scala stream
// */
//class WrapStream[T](stream: Stream[T])
//	extends Infinite[T] {
//  
//  override def apply(ind: Int) =
//    stream(ind)
//    
//}