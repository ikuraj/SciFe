package insynth.enumeration
package reverse
package dependent

import insynth.enumeration.dependent._
import insynth.{ enumeration => e }
import insynth.enumeration.lzy._

import insynth.util.logging._

class ChainFinite[I, O]
  (override val left: Reverse[I], override val right: DependReverse[I, O])
  extends lzy.ChainFinite(left, right) with Finite[(I, O)] with Reverse[(I, O)] {
  
  def reverse[T >: (I, O)](a: T): Finite[(I, O)] = {
    val (in, out) = a.asInstanceOf[(I, O)]
    val leftReverse = left.reverse(in)
    val rightReverse = right(in).reverse(out)
    
    val beginningPart =
      e.lzy.Product(in, rightReverse)
    if (leftReverse.size > 1)
	    e.Concat(
        beginningPart,
//	  		e.Map(rightReverse, { (in, _: O) }),
	  		new e.dependent.lzy.ChainFinite(leftReverse, right)
	    )
    // if we hit the last outer stream
    else
      beginningPart
//      e.Map(rightReverse, { (in, _: O) })
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