package insynth.enumeration
package reverse
package dependent

import insynth.{ enumeration => e }
import e.{ dependent => dp }

import insynth.util.logging._

class ChainFinite[I, O]
  (override val left: Reverse[I], override val right: DependReverse[I, O])
  extends dp.lzy.ChainFinite(left, right) with Reverse[(I, O)] {
  
  def reverse[T >: (I, O)](a: T) = {
    val (leftIn, rightIn) = a.asInstanceOf[(I, O)]

    val leftInd = left.reverse(leftIn)
    
    val beginningPart = {
      var size = 0
      for( ind <- leftInd until left.size; innerEnum = right( left(ind) ) )
        size += innerEnum.size

      size
    }

    val rightDepPar = left(leftInd)
    beginningPart + right.apply( rightDepPar ).reverse( rightIn, rightDepPar )
  }

}

//class ChainFiniteCombine[I, O, R]
//  (val left: Finite[I], val right: DependFinite[I, O], combine: (I, O) => R = (_: I, _: O))
//  extends Finite[R] with HasLogger {
//  
//  val rr = ConcatFinite.buffer[R]( Seq.empty )
//  
//  override def size: Int = {
//    var size = 0
//    for( ind <- 0 until left.size; innerEnum = right( left(ind) ) ) yield
//      if ( innerEnum.size == -1 ) return -1
//      else size += innerEnum.size
//    size
//  }
//  
//  var explored = -1
//  
//  override def apply(ind: Int) = {
//    while(ind >= rr.size) {
//      explored += 1
//      if (explored >= left.size) throw new NoSuchElementException("Went out of range of this lazy structure")
//      val leftProduced = left(explored)
//      val righEnum = right(leftProduced)
//      val toAdd = e.Map(righEnum, { (rightProduced: O) => combine(leftProduced, rightProduced) })
//      rr.append( toAdd )
//    }
//    rr(ind)
//  }
//  
//}