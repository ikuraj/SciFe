package scife.enumeration
package lazytraversal
package split

import combinators._
import scife.{ enumeration => e }

import scala.collection.mutable._

import scife.util._

class ChainFiniteSingleCombineInner[I, O, R]
  (val left: Finite[I], val right: LazyDependFinite[I, O], val combine: (I, => O) => R)
  (implicit val ct: scala.reflect.ClassTag[I])
  extends
    //e.dependent.ChainFiniteSingleCombine(left, right, combine) with
    Finite[R] with HasLogger {
  
  var enumArray: Array[LazyDependFinite[I, O]#EnumType] = _
  var leftArray: Array[I] = _
  
  val limits = {
    var _size = 0
    val ab = ArrayBuffer(0)
    val buff = ArrayBuffer.empty[LazyDependFinite[I, O]#EnumType]
    val leftBuff = ArrayBuffer.empty[I]
    for (ind <- 0 until left.size; leftEl = left(ind);
        stream = right.getEnum(leftEl); if stream.size > 0) {
      buff += stream
      leftBuff += leftEl
      _size += stream.size
      ab += _size
    }
    enumArray = buff.toArray
    leftArray = leftBuff.toArray
    ab.toArray
  }
  assert(limits.size == enumArray.length + 1)

  override def apply(ind: Int) = {
    assert(ind < size)
    val arrInd = binarySearch(ind)
    val elInd = ind - limits(arrInd)
    
//    assert(binarySearch(0) != binarySearch(1), s"${binarySearch(0)}, ${binarySearch(1)}")
//    val flag = if (ind == 1) arrInd == 1 else true
//    assert(flag)
    
    // mark as untouched
//    enumArray(arrInd).touched = false
//    println(s"unoutch:${enumArray(arrInd).hashCode}")
    
//    val leftV = left(arrInd)
//    val rightV = enumArray(arrInd)(elInd)
//    combine(leftV, rightV)
    combine(leftArray(arrInd), {
//      println(s"invoke from $this, with ${leftArray(arrInd)}, $elInd"); enumArray(arrInd)(elInd) 
//      enumArray(arrInd)(elInd) 
      val res= enumArray(arrInd)(elInd);  enumArray(arrInd)._touched = true; res
    })
  }
  
  override def size = {
    limits.apply(enumArray.length)
  }

  private[enumeration] def binarySearch(target: Int): Int = {
    var left = 0
    // limits are indexed 0..length
    var right = enumArray.length
    while (left <= right) {
      val mid = (left + right) / 2
      info("target=%d, left=%d, mid=%d, right=%d".format(target, left, mid, right))
      if (limits(mid) <= target && limits(mid + 1) > target)
        return mid
      else if (limits(mid) > target)
        right = mid
      else
        left = mid
    }
    // should not happen
    throw new RuntimeException
  }
  
  override def toString =
    s"ChainFinite[$hashCode](l=${left.toList},r=${right.hashCode()}"

}