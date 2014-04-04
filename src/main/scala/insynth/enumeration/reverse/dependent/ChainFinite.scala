package insynth.enumeration
package reverse
package dependent

import insynth.{ enumeration => e }
import e.{ dependent => dp }

import insynth.util.logging._

class ChainFinite[I, O]
  (override val left: Reverse[I], override val right: DependReverse[I, O])
  extends dp.lzy.ChainFinite(left, right) with Reverse[(I, O)] with HasLogger {
  
  def reverse[T >: (I, O)](a: T) = {
    val (leftIn, rightIn) = a.asInstanceOf[(I, O)]
    info("(leftIn, rightIn)=" + (leftIn, rightIn))

    val leftInd = left.reverse(leftIn)
    
    val beginningPart = {
      var size = 0
      for( ind <- 0 until leftInd; innerEnum = right( left(ind) ) )
        size += innerEnum.size

      size
    }

//    val rightDepPar = left(leftInd)
    val rightDepPar = leftIn
    info("Beginning part is %d. Right part is %s (under %s).".format(beginningPart,
      rightIn, rightDepPar))
    info("right reversed" + right.apply( rightDepPar ).reverse( rightIn ))
    beginningPart + right.apply( rightDepPar ).reverse( rightIn )
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