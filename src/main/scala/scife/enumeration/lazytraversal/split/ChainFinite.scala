package scife.enumeration
package lazytraversal
package split

import combinators._
import scife.{ enumeration => e }

import scala.collection.mutable._

import scife.util._

class ChainFiniteSingleCombine[I, O, R]
  (left: Finite[I], val right: LazyDependFinite[I, O], combine: (I, => O) => R)
  (_inner: ChainFiniteSingleCombineInner[I, O, R] = null)
  (implicit ct: scala.reflect.ClassTag[I])
  extends
    //e.dependent.ChainFiniteSingleCombine(left, right, combine) with
    Finite[R] with Skippable[R] with Resetable[R] with HasLogger {
  
//  def this(in: ChainFiniteSingleCombineInner[I, O, R], r: LazyDependFinite[I, O]) =
//    this(in.left, r, in.combine)(in)(in.ct)
  
  val inner =
    if (_inner == null)
      new ChainFiniteSingleCombineInner(left, right, combine)
    else _inner
  
  import inner._
  
  // TODO optimize two times search
  override def next(ind: Int) = {
    val arrInd = binarySearch(ind)
    val elInd = ind - limits(arrInd)
    
//    println(s"arrInd=$arrInd, elInd=$elInd")
//    println(s"touched=${enumArray.map(_.touched).mkString(",")}")
//    println(s"limits=${limits.mkString(",")}")
    if (!enumArray(arrInd).touched) limits(arrInd + 1)
    else {
      val innerNext = enumArray(arrInd).next(elInd)
//      println(s"innerArray=${enumArray(arrInd)}")
//      println(s"innerNext=$innerNext")
      if (innerNext <= enumArray(arrInd).size) limits(arrInd) + innerNext
      else {
        limits(arrInd + 1)
      }
    }
  }

  override def apply(ind: Int) = {
    inner.apply(ind)
  }
  
  override def reset = {
    super.reset
    for (en <- enumArray; if en.touched) en.reset
  }
  
  override def size = inner.size
  
  override def hardReset {
//    super.hardReset
    for (en <- enumArray) en.hardReset
//    println(s"hard reset in $this")
  }

}