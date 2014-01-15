package insynth.streams

import org.scalatest.matchers._

object Utils extends ShouldMatchers {
  
  def compareCallsToGetStream[T](s: Streamable[T], maxCalls: Int = 20) {
    val s1 = s.getStream
    val s2 = s.getStream
    
    for (ind <- 1 to maxCalls; toTake = 50 * ind) {
      s1.take(toTake) should equal ( s2.take(toTake) )
      s1.take(toTake) should equal ( s.getStream.take(toTake) )
    }
  }
  
  def compareCallsToGetStream[T](coll: Traversable[Streamable[T]]) {
    for (s <- coll)
      compareCallsToGetStream(s)
  }
  
  def compareCallsToGetStream(coll: Traversable[Streamable[Int]], maxCalls: Int) {
    for (s <- coll)
      compareCallsToGetStream(s, maxCalls)
  }
  
}