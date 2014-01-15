package insynth.streams.ordered

import org.scalatest.matchers._

import insynth.streams._

object Utils extends ShouldMatchers {
  
  def compareCallsToGetStream[T](s: IntegerWeightStreamable[T], maxCalls: Int = 20) {
    val s1 = s.getValuedStream
    val s2 = s.getValuedStream
    
    for (ind <- 1 to maxCalls; toTake = 50 * ind) {
      s1.take(toTake) should equal ( s2.take(toTake) )
      s1.take(toTake) should equal ( s.getValuedStream.take(toTake) )
    }
  }
  
  def compareCallsToGetStream[T](coll: Traversable[IntegerWeightStreamable[T]]) {
    for (s <- coll)
      compareCallsToGetStream(s)
  }
  
  def compareCallsToGetStream(coll: Traversable[IntegerWeightStreamable[Int]], maxCalls: Int) {
    for (s <- coll)
      compareCallsToGetStream(s, maxCalls)
  }
  
  // dummy here for these pesky erasure errors
  def getSingleStream[T](streamToUse: => Stream[(T, Int)], flag: Boolean = false)(implicit s:DummyImplicit): IntegerWeightStreamable[T] =
    WrapperStream(streamToUse, flag)
  
  def getSingleStream(streamToUse: => Stream[Int], flag: Boolean): IntegerWeightStreamable[Int]  =
    getSingleStream(streamToUse zip streamToUse, flag)
  
  def getSingleStream(list: List[Int]): IntegerWeightStreamable[Int] =
  	getSingleStream(list.sortWith(_<_).toStream, false)
  
  def getSingleStream(el: Int): IntegerWeightStreamable[Int] =
  	getSingleStream(Stream(el), false)

  def streamToString[A](stream: Stream[A])(n: Int) = stream.take(n).toList mkString (", ")
  	
}