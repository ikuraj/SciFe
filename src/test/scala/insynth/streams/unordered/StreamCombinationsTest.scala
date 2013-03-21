package insynth.streams.unordered

import scala.util.Random

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.Test

import insynth.streams.Streamable

class StreamCombinationsTest {

  val rnd = new Random(System.currentTimeMillis())
  
  @Test
  def testLazyRoundRobbinAndBinaryStreamLoop {
    val stream1 = Singleton(1)

    val rr = LazyRoundRobbin(List(stream1))

    val stream2 = SingleStream(Stream(3), false)

    val bs = BinaryStream(rr, stream2) {
      (x, y) => x + y
    }

    rr.streams :+= bs

    rr.initialize

    val stream = rr.getStream
    
    lazy val streamMessage = "Stream contents: " + stream.take(20).mkString(", ") 

    for (ind <- 1 to rnd.nextInt(1000)) {
      assertEquals(streamMessage, 1 + (3 * ind), stream(ind))
    }
  }

  @Test
  def testLazyRoundRobbinAndBinaryStreamLoop2 {
    val stream1 = Singleton(1)

    val rr = LazyRoundRobbin(List(stream1))

    val stream2 = SingleStream(Stream(3, 2), false)

    val bs = BinaryStream(stream2, rr) {
      (x, y) => x - y
    }

    rr.streams :+= bs

    rr.initialize

    val stream = rr.getStream
    
    lazy val streamMessage = "Stream contents: " + stream.take(20).mkString(", ") 

    assertEquals(streamMessage, 1, stream(0))
    assertEquals(streamMessage, 2, stream(1))
    
    // cannot know in which order so guarantee that all elements will be there with same number of occurences
    val randomValue = rnd.nextInt(1000) * 2
    
    val occurenceMap =
    (Map[Int, Int]() /: (1 to randomValue)) {
      (map, el) => map + ( stream(el) -> (map.getOrElse(stream(el), 0) + 1) )
    }
    
    val resultMap =
    (occurenceMap /: (1 to (randomValue/2))) {
      (map, el) =>
        val v1 = 3 - stream(el - 1)
        val v2 = 2 - stream(el - 1)
        map + ( v1 -> (map.getOrElse(v1, 0) - 1) ) + ( v2 -> (map.getOrElse(v2, 0) - 1) )
    }
    
    assertTrue("ResultMap: " + resultMap.mkString(", "), resultMap.values.count(_ != 0) < 3)
    assertTrue("ResultMap: " + resultMap.mkString(", "), (0 /: resultMap.values.filter(_ != 0)) { _ + math.abs(_) } < 3)
    
  }

  @Test
  def testLazyRoundRobbinAndBinaryStreamLoop3 {

    val streamable1 = Singleton(1)
    val streamable2 = Singleton(2)

    val streamable3 = LazyRoundRobbin(List(streamable1))
    val streamable4 = LazyRoundRobbin(List(streamable2))

    streamable3.initialize
    streamable4.initialize

    val bs = BinaryStream(streamable3, streamable4) {
      (x, y) => x + y
    }

    val stream = bs.getStream

    assertFalse(bs.isInfinite)
    assertEquals(
      stream.take(1) mkString (", "),
      List(3),
      stream.take(1).toList)

    assertEquals(1, stream.size)
  }

}