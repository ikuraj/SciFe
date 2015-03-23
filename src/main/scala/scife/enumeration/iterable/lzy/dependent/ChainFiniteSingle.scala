package scife.enumeration
package iterable
package lzy
package dependent

import combinators._
import scife.{ enumeration => e }

import scife.util._

import scala.reflect.ClassTag

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
  (left: LazyEnumFinite[I], val right: LazyDependFinite[I, O], combine: (=> I, => O) => R)
  (implicit ct: ClassTag[I])
  extends
    //e.dependent.ChainFiniteSingleCombine(left, right, combine) with
    Finite[R] with ResetIter[R] with HasLogger {

  type E = right.EnumType
  
  var categoryIndex = 0
  var _size = 0
  
  val (rightStreams, leftValues): (Array[E], Array[I]) = {
    val streams =
      for (
//          ind <- 0 until left.size; leftVal = left(ind);
          ind <- 0 until left.size; leftVal = left.next;
          stream = right.getEnum(leftVal); if stream.size > 0)
        yield {
        _size += stream.size
          (stream, leftVal)
        }
    info(s"streams=${streams}")
    assert(!left.hasNext)
    left.reset
          
    val (_rightStreams, lefts) = streams.unzip
    
    ( _rightStreams.toArray, lefts.toArray)
  }
  info(s"rightStreams.size=${rightStreams.size}")
  
  val sizeArray = rightStreams map { _.size }
  info(s"sizeArray size=${sizeArray.mkString(" ")}")

  override def head = combine( leftValues(categoryIndex), rightStreams(categoryIndex).head )

  override def hasNext =
    rightStreams(categoryIndex).touched && rightStreams(categoryIndex).hasNext ||
      categoryIndex < rightStreams.size - 1
  
  override def next = {
    require(hasNext)
    info(s"[next:] categoryIndex=$categoryIndex/${rightStreams.size}, touched=${rightStreams(categoryIndex).touched}")
    if (rightStreams(categoryIndex).touched) {
      // check if going to next element switches to next inner enum
      if (!rightStreams(categoryIndex).hasNext) {
        categoryIndex += 1
      }
    }
    else {
      categoryIndex += 1
    }
    info(s"invoking super.next with $ind")
//    super.next
    rightStreams(categoryIndex).touched = false
    info(s"leftValues(categoryIndex)=${leftValues(categoryIndex).toString}")
    combine( leftValues(categoryIndex), rightStreams(categoryIndex).next )   
  }

  override def size = _size

  override def apply(ind: Int): R =
    // does not work well with touched -- need to decouple
    throw new RuntimeException
    
  // TODO refactor this when inner enumerators are reset iter, than this can work
  override def hasStarted = throw new RuntimeException//leftValues(categoryIndex).hasStarted
  
  override def reset = {
    // can do this along the way (during next), but this is cleaner
    for (i <- 0 to categoryIndex)
      rightStreams(i).reset
    categoryIndex = 0
  }
    
}

class ChainFiniteSingleCombineStrict[I, O, R]
  (left: LazyEnumFinite[I], val right: LazyDependFinite[I, O], combine: (I, O) => R)
  (implicit ct: ClassTag[I])
  extends
    //e.dependent.ChainFiniteSingleCombine(left, right, combine) with
    Finite[R] with ResetIter[R] with HasLogger {

  type E = right.EnumType
  
  var categoryIndex = 0
  var _size = 0
  
  val (rightStreams, leftValues): (Array[E], Array[I]) = {
    val streams =
      for (
//          ind <- 0 until left.size; leftVal = left(ind);
          ind <- 0 until left.size; leftVal = left.next;
          stream = right.getEnum(leftVal); if stream.size > 0)
        yield {
        _size += stream.size
          (stream, leftVal)
        }
    info(s"streams=${streams}")
    assert(!left.hasNext)
    left.reset
          
    val (_rightStreams, lefts) = streams.unzip
    
    ( _rightStreams.toArray, lefts.toArray)
  }
  info(s"rightStreams.size=${rightStreams.size}")
  
  val sizeArray = rightStreams map { _.size }
  info(s"sizeArray size=${sizeArray.mkString(" ")}")

  override def head = combine( leftValues(categoryIndex), rightStreams(categoryIndex).head )

  override def hasNext =
    rightStreams(categoryIndex).touched && rightStreams(categoryIndex).hasNext ||
      categoryIndex < rightStreams.size - 1
  
  override def next = {
    require(hasNext)
    info(s"[next:] categoryIndex=$categoryIndex/${rightStreams.size}, touched=${rightStreams(categoryIndex).touched}")
    if (rightStreams(categoryIndex).touched) {
      // check if going to next element switches to next inner enum
      if (!rightStreams(categoryIndex).hasNext) {
        categoryIndex += 1
      }
    }
    else {
      categoryIndex += 1
    }
    info(s"invoking super.next with $ind")
//    super.next
    rightStreams(categoryIndex).touched = false
    info(s"leftValues(categoryIndex)=${leftValues(categoryIndex).toString}")
    combine( leftValues(categoryIndex), rightStreams(categoryIndex).next )   
  }

  override def size = _size

  override def apply(ind: Int): R =
    // does not work well with touched -- need to decouple
    throw new RuntimeException
    
  // TODO refactor this when inner enumerators are reset iter, than this can work
  override def hasStarted = throw new RuntimeException//leftValues(categoryIndex).hasStarted
  
  override def reset = {
    // can do this along the way (during next), but this is cleaner
    for (i <- 0 to categoryIndex)
      rightStreams(i).reset
    categoryIndex = 0
  }
    
}

//class ChainFiniteSingleCombine[I, O, R]
//  (left: LazyEnumFinite[I], val right: LazyDependFinite[I, O], combine: (=> I, => O) => R)
//  extends
//    //e.dependent.ChainFiniteSingleCombine(left, right, combine) with
//    Finite[R] with Iter[R] with HasLogger {
//
//  type E = right.EnumType
//  
//  var categoryIndex = 0
//  
//  val (rightStreams, streamArray): (Array[E], Array[Finite[R]]) = {
//    val streams =
//      for (ind <- 0 until left.size; leftVal = left(ind);
//          stream = right.getEnum(leftVal); if stream.size > 0)
//        yield {
//          val rightProduce: (=> O) => R = (rightProduced) => combine(leftVal, rightProduced)
//          (stream, e.LazyMap(stream, rightProduce))
//        }
//    info(s"streams=${streams}")
//          
//    val (_rightStreams, finalStreams) = streams.unzip
//    
//    ( _rightStreams.toArray, Array( finalStreams: _*))
//  }
//  info(s"streamArray.size=${streamArray.size}")
//  info(s"rightStreams.size=${rightStreams.size}")
//  
//  val sizeArray = (streamArray map { _.size }).scanLeft(0) { _ + _ }.tail
//  info(s"sizeArray size=${sizeArray.mkString(" ")}")
//
//  val rr = new ConcatFiniteVariedSize(streamArray)
//  
//  override def hasNext = {
//    if (categoryIndex < rightStreams.size) {
//      info(s"[hasNext:] categoryIndex=$categoryIndex" +
//        s", rightStreams(categoryIndex).touched=${rightStreams(categoryIndex).touched}")
//      if (rightStreams(categoryIndex).touched)
//        super.hasNext
//      else true
//    }
//    else false
//  }
//  
//  override def next = {
//    info(s"[next:] categoryIndex=$categoryIndex/${rightStreams.size}, touched=${rightStreams(categoryIndex).touched}")
//    if (rightStreams(categoryIndex).touched) {
//      // check if going to next element switches to next inner enum
//      if (ind >= sizeArray(categoryIndex) - 1)
//        categoryIndex += 1
//    }
//    else {
//      ind = sizeArray(categoryIndex) - 1
//      info(s"ind set to $ind")
//      categoryIndex += 1
//    }
//    info(s"invoking super.next with $ind")
//    super.next
//  }
//
//  override def size = rr.size
//
//  override def apply(ind: Int) =
//    // does not work well with touched -- need to decouple
//    rr(ind)
//}