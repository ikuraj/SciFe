package scife.enumeration
package lzy
package breadth

import combinators._

import scala.collection.mutable
import scala.reflect.ClassTag

import scife.util._

class ConcatFiniteVariedSize[@specialized T] protected[enumeration] (val enumArray: Array[Finite[T]])
  extends Finite[T] with ConcatMul[T] with HasLogger {

  override def enums = enumArray.toSeq

  override def apply(ind: Int) = {
    println("apply", ind)
    val sectorInd = binarySearch(ind)
    println("sectorInd=" + sectorInd)
    val (enumOffset, numEnums, _) = metadata(sectorInd)
    println("meta" + metadata(sectorInd))
    val indWithinSector = ind - limits(sectorInd)
    println("limits(sectorInd)=" + limits(sectorInd))
    val arrayInd = indWithinSector % numEnums
    val arrayOffset = indWithinSector / numEnums
    
    println(s"arrayInd=$arrayInd, enumOffset + arrayOffset=${enumOffset + arrayOffset}")
    sortedEnums(arrayInd)(enumOffset + arrayOffset)
  }
  
  private[this] var sortedEnums: Array[Finite[T]] = _
  private[this] var metadata: Array[(Int, Int, Int)] = _
  
  
  private[this] val limits = {
    import mutable._

//    implicit def orderingBySize: Ordering[Finite[T]] = Ordering.by(e => e.size)
//    implicit object EnumOrdering extends Ordering[Finite[T]] {
//      def compare(a: Finite[T], b: Finite[T]) =
//        if (a.size > b.size) 1 else -1 
//    }
//
    sortedEnums = enumArray sortWith { (a, b) => a.size > b.size }
    println(s"sortedEnums=${sortedEnums.map(_.size).mkString(", ")}")
    
    var currOffset = 0
    var leftEnums = enums.size
    val metadata = mutable.ArrayBuffer.empty[(Int, Int, Int)]

    var currBound = 0
    val bounds = mutable.ArrayBuffer(0)

    val enumMap = sortedEnums.groupBy(e => e.size)
    val sortedKeys = SortedSet(enumMap.keys.toSeq: _*)
    for (enumSize <- sortedKeys) {
      val bucketSize = enumMap(enumSize).size
      val tuple = (currOffset, leftEnums, bucketSize)
      println(s"tuple=$tuple")
      println(s"bucketSize=$bucketSize, enumSize=$enumSize")
      
      // search bound
      currBound += leftEnums * (enumSize - currOffset)
      bounds += currBound

      // update metadata
      leftEnums -= bucketSize
      currOffset += (enumSize - currOffset)
      
      metadata += tuple
    }
    println(s"bounds=$bounds")
    println(s"meta=$metadata")
    
    this.metadata = metadata.toArray

    bounds.toArray
  }
  
//  private[this] val limits = {
//    import mutable._
////    implicit def orderingBySize: Ordering[Finite[T]] = Ordering.by(e => e.size)
////    object EnumOrdering extends Ordering[Finite[T]] {
////      def compare(a: Finite[T], b: Finite[T]) =
////        if (a.size > b.size) 1 else -1 
////    }
////
////    implicit val config = SortedBagConfiguration.keepAll(EnumOrdering)
//  
//    val enumSizeBag = TreeBag.empty
//    enumSizeBag ++= enumArray
//    assert(enumSizeBag.size == 5)
//    
//    sortedEnums = enumSizeBag.toArray
//    
//    var currOffset = 0
//    var leftEnums = enums.size
//    val metadata = mutable.ArrayBuffer.empty[(Int, Int, Int)]
//
//    var currBound = 0
//    val bounds = mutable.ArrayBuffer(0)
//
//    assert(enumSizeBag.bucketsIterator.size == 5) 
//    for (bucket <- enumSizeBag.bucketsIterator) {
//      val tuple = (currOffset, leftEnums, bucket.size)
//      println(s"tuple=$tuple")
//      println(s"bucket=$bucket, bucket.size=${bucket.size}, head.size=${bucket.head.size}")
//      
//      // search bound
//      currBound += bucket.size * bucket.head.size
//      bounds += currBound
//
//      // update metadata
//      leftEnums -= bucket.size
//      currOffset += bucket.head.size
//      
//      metadata += tuple
//    }
//    assert(bounds.size == 3, s"bounds=$bounds")
//    
//    this.metadata = metadata.toArray
//
//    bounds.toArray
//  }

  override def size = {
    limits.last
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

}