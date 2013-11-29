package insynth.streams.ordered

import insynth.streams._

object Utils {
  
  // dummy here for these pesky erasure errors
  def getSingleStream[T](streamToUse: => Stream[(T, Int)], flag: Boolean = false)(implicit s:DummyImplicit): OrderedStreamable[T] =
    TestSingleStream(streamToUse, flag)
//    if (flag) TestSingleStream(streamToUse, flag)
//    else FiniteStream(streamToUse.toVector)
  
  def getSingleStream(streamToUse: => Stream[Int], flag: Boolean): OrderedStreamable[Int]  =
    getSingleStream(streamToUse zip streamToUse, flag)
  
  def getSingleStream(list: List[Int]): OrderedStreamable[Int] =
  	getSingleStream(list.sortWith(_<_).toStream, false)
  
  def getSingleStream(el: Int): OrderedStreamable[Int] =
  	getSingleStream(Stream(el), false)

  def streamToString[A](stream: Stream[A])(n: Int) = stream.take(n).toList mkString (", ")
  	
}