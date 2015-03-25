package scife.enumeration
package lazytraversal
package split

import scife.enumeration.memoization.Memoizable
import scife.enumeration.dependent.Depend

import scalaz.LazyTuple2
import scife.util.structures.LazyBSTrees._

import scala.collection.mutable

trait Memoized[I, O, U] extends Depend[(I, U), O] with Memoizable {
  
//  type U = ((Int, Range), split.ChainFiniteSingleCombineInner[(Int, Int), LazyTuple2[Tree, Tree], Tree])
//  type I = (Int, Range)
  
  private[enumeration] val memoizedMap = mutable.Map[I, U]()
  
  override abstract def getEnum(_parameter: (I, U)) = {
    val (parameter, ug) = _parameter
//    println(s"memoizedMap.contains($parameter) " + memoizedMap.contains(parameter))
    //memoizedMap.getOrElseUpdate(parameter, super.getEnum(parameter))
    if (memoizedMap contains parameter) {
//      println(s"memoizedMap getting ${memoizedMap(parameter)}")
      val res = memoizedMap(parameter)
      res match {
        case cf: ChainFiniteSingleCombine[(Int, Range), LazyTuple2[Tree, Tree], Tree] =>
          super.getEnum(parameter, res)
        case r: EnumType =>
          r
      }
//      res match {
//        case cf: ChainFiniteSingleCombine[(Int, Range), LazyTuple2[Tree, Tree], Tree] =>
//          cf.right match {
//            case pf: ProductFiniteLazyTuple[(Int, Range), Tree] =>
//              val newPf  =
//                new ProductFiniteLazyTuple(pf.inner)
//              
//              new ChainFiniteSingleCombine[(Int, Range), LazyTuple2[Tree, Tree], Tree](
//                cf.inner, cf.right).asInstanceOf[EnumType]
//          }
//        case _: Touchable[O] =>
//          super.getEnum(parameter)
//        case _ =>
//          res
//      }
//      super.getEnum(parameter, res)
    }
    else {
      val res = super.getEnum(_parameter)
      memoizedMap(parameter) = res.asInstanceOf[U]
      res
    }
  }

  override def clearMemoization = memoizedMap.clear
  
  // helper (debugging) method
  protected[enumeration] def isMemoized(el: I) = memoizedMap contains el
  
}
