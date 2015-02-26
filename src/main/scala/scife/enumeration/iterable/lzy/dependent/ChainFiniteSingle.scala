package scife.enumeration
package iterable
package lzy
package dependent

import combinators._
import scife.{ enumeration => e }

import scife.util._

class ChainFiniteSingle[I, O]
  (override val left: LazyEnumFinite[I], override val right: LazyDependFinite[I, O])
  extends e.dependent.ChainFiniteSingle[I, O](left, right) with Finite[O]
    with ResetIter[O] with Touchable[O] with HasLogger {
  
  // TODO
  var categoryIndex = 0
  
  val streamArray = {
    val rightStreams =
      for (ind <- 0 until left.size; stream = right.getEnum(left(ind)); if stream.size > 0 )
        yield stream
    
    Array(rightStreams: _*)
  }

  override val rr = new ConcatFiniteVariedSize(streamArray)

//  override def next = {
////    if (false)
//    super.next
//  }

}

class ChainFiniteSingleCombine[I, O, R]
  (left: LazyEnumFinite[I], val right: LazyDependFinite[I, O], combine: (I, O) => R)
  extends e.dependent.ChainFiniteSingleCombine(left, right, combine) with Finite[R]
    with ResetIter[R] with Touchable[R] with HasLogger {

  type E = right.EnumType
  
  var categoryIndex = 0
  
  val (rightStreams, streamArray): (Array[E], Array[Finite[R]]) = {
    val streams =
      for (ind <- 0 until left.size; leftVal = left(ind);
          stream = right.getEnum(leftVal); if stream.size > 0 )
        yield
          (stream, e.Map( stream, { (rightProduced: O) => combine(leftVal, rightProduced) }))
          
    val (_rightStreams, finalStreams) = streams.unzip
    
    ( _rightStreams.toArray, Array( finalStreams: _*))
  }
  
  val sizeArray = (streamArray map { _.size }).scanLeft(0) { _ + _ }

  override val rr = new ConcatFiniteVariedSize(streamArray)
  
  override def hasNext =
    if (rightStreams(categoryIndex).touched)
      super.hasNext
    else categoryIndex < rightStreams.size - 1
  
  override def next = {
    if (rightStreams(categoryIndex).touched) {
      if (ind >= sizeArray(categoryIndex) - 1)
        categoryIndex += 1
    }
    else {
      ind = sizeArray(categoryIndex) - 1
      categoryIndex += 1
//      this.apply(ind)
    }
    super.next
  }

}