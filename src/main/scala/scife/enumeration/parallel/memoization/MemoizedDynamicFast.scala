//package scife.enumeration
//package parallel
//package memoization
//
//import scife.enumeration.memoization.Memoizable
//
//import scala.collection.mutable.BitSet
//import java.util.Vector
//
//import scife.util._
//
//trait MemoizedDynamicFast[T >: Null] extends Enum[T] with Memoizable with HasLogger {
//
////  protected[enumeration] val memoizedFlags = new BitSet()
//  protected[enumeration] val memoizedValues = new Vector[T](this.size)
//
//  override abstract def apply(ind: Int) = {
//    if (memoizedValues.size < ind && memoizedValues.get(ind) != null)
//      memoizedValues.get(ind)
//    else {
//      val nextValue = super.apply(ind)
//      for (i <- 1 to ind - memoizedValues.size)
//        memoizedValues.add(null)
//      memoizedValues.add(nextValue) 
//      nextValue
//    }
//  }
//
//  override def clearMemoization {
//    memoizedValues.clear
//  }
//
//}
