//package insynth.streams
//package dependent
//
//import scala.collection.mutable
//
//import light._
//
//trait Memoized[I, O] extends Depend[I, O] with Memoizable {
//
//  val memoizedMap = mutable.Map[I, Enum[O]]()
//  
//  override abstract def getStream(parameter: I) = {
//    memoizedMap.getOrElseUpdate(parameter, super.getStream(parameter))
//  }
//  
//  override def clearMemoization = memoizedMap.clear
//  
//}
//
//// should return all finite/infinite enumerables
//
////trait FiniteDepend[I, +O] extends Depend[I, O] {
////
////  override def getStream(parameter: I): Finite[O]
////  
////}
////
////trait InfiniteDepend[I, +O] extends Depend[I, O] {
////
////  override def getStream(parameter: I): Infinite[O]
////  
////}