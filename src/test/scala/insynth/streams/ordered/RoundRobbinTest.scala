package insynth.streams.ordered

import scala.util.Random

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.Test

class RoundRobbinTest extends JUnitSuite {

  import Utils._

  @Test
  def testRoundRobbin1 {
    val finiteStream1 = List(1, 2, 3)
    val finiteStream2 = List(4, 5, 6)

    val streamable1 = getSingleStream(finiteStream1)
    val streamable2 = getSingleStream(finiteStream2)

    val rr = RoundRobbin(IndexedSeq(streamable1, streamable2))

    val stream = rr.getStream

    val streamString = streamToString(stream)(6)

    assertFalse(streamString, rr.isInfinite)

    assertEquals(streamString, 6, stream.size)

    assertEquals(streamString, List(1, 2, 3, 4), stream.take(4))

    compareCallsToGetStream(List(streamable1, streamable2, rr))
  }

  @Test
  def testRoundRobbin2 {

    val finiteStream2 = List(1, 2, 3)
    val finiteStream1 = List(4, 5, 6)

    val streamable1 = getSingleStream(finiteStream1)
    val streamable2 = getSingleStream(finiteStream2)

    val rr = RoundRobbin(IndexedSeq(streamable1, streamable2))

    val stream = rr.getStream

    val streamString = streamToString(stream)(6)

    assertFalse(streamString, rr.isInfinite)

    assertEquals(streamString, 6, stream.size)

    assertEquals(streamString, List(1, 2, 3, 4), stream.take(4))

    compareCallsToGetStream(List(streamable1, streamable2, rr))
  }

  @Test
  def testRoundRobbin3 {
    val finiteStream1 = List(1, 5, 3)
    val finiteStream2 = List(4, 2, 6)

    val streamable1 = getSingleStream(finiteStream1)
    val streamable2 = getSingleStream(finiteStream2)

    val rr = RoundRobbin(IndexedSeq(streamable1, streamable2))

    val stream = rr.getStream

    val streamString = streamToString(stream)(6)

    assertFalse(streamString, rr.isInfinite)

    assertEquals(streamString, 6, stream.size)

    assertEquals(streamString, List(1, 2, 3, 4), stream.take(4))

    compareCallsToGetStream(List(streamable1, streamable2, rr))
  }

  @Test
  def testRoundRobbin4 {
    val rnd = new Random(System.currentTimeMillis())
    val randomInt1 = rnd.nextInt
    val randomInt2 = rnd.nextInt

    val infiniteStream1 = Stream.continually(randomInt1)
    val infiniteStream2 = Stream.continually(randomInt2)

    val streamable1 = getSingleStream(infiniteStream1, true)
    val streamable2 = getSingleStream(infiniteStream2, true)

    val rr = RoundRobbin(Array(streamable1, streamable2))

    val stream = rr.getStream

    val streamString = streamToString(stream)(6)

    assertTrue(streamString, rr.isInfinite)

    assertEquals(streamString, 100, stream.take(100).size)

    assertEquals(streamString, List.fill(4)(randomInt1 min randomInt2), stream.take(4))

    compareCallsToGetStream(List(streamable1, streamable2, rr))
  }

  @Test
  def testRoundRobbin5 {
    val randomInt1 = 100

    val infiniteStream1 = Stream.continually(randomInt1)
    val finiteStream2 = List(4, 5, 6).toStream

    val streamable1 = getSingleStream(infiniteStream1, true)
    val streamable2 = getSingleStream(List(4, 5, 6).toStream, false)

    val rr = RoundRobbin(Array(streamable1, streamable2))

    val stream = rr.getStream

    val streamString = streamToString(stream)(6)

    assertTrue(streamString, rr.isInfinite)

    assertEquals(streamString, 100, stream.take(100).size)

    val compareList = List(4, 5, 6, randomInt1, randomInt1, randomInt1)

    assertEquals(streamString, compareList, stream.take(compareList.size))

    compareCallsToGetStream(List(streamable1, streamable2, rr))
  }

  @Test
  def testRoundRobbin6 {
    
      val finiteStream1 = List(3).toStream
      val finiteStream2 = List(1).toStream
      val finiteStream3 = List(2).toStream

      val streamable1 = getSingleStream(finiteStream1, false)
      val streamable2 = getSingleStream(finiteStream2, false)
      val streamable3 = getSingleStream(finiteStream3, false)

      val rr = RoundRobbin(Array(streamable1, streamable2, streamable3))

      val stream = rr.getStream

      assertFalse(rr.isInfinite)

      assertEquals(3, stream.size)

      assertEquals(List(1, 2, 3), stream.take(3))

      compareCallsToGetStream(List(streamable1, streamable2, streamable3, rr))
  }

  @Test
  def testRoundRobbin7 {
    
      // cannot be singletons (they return 1 by default)
      //	    val streamable1: Streamable[Int] = Singleton(2)
      //	    val streamable2: Streamable[Int] = Singleton(3)
      //	    val streamable3: Streamable[Int] = Singleton(1)
      val finiteStream1 = List(2).toStream
      val finiteStream2 = List(3).toStream
      val finiteStream3 = List(1).toStream

      val streamable1 = getSingleStream(finiteStream1, false)
      val streamable2 = getSingleStream(finiteStream2, false)
      val streamable3 = getSingleStream(finiteStream3, false)

      val rr = RoundRobbin(Array(streamable1, streamable2, streamable3))

      val stream = rr.getStream

      assertFalse(rr.isInfinite)

      assertEquals(3, stream.size)

      assertEquals(List(1, 2, 3), stream.take(3))

      compareCallsToGetStream(List(streamable1, streamable2, streamable3, rr))
  }

  @Test
  def testRoundRobbin8 {
    
      val streamable1 = getSingleStream(1)
      val streamable2 = getSingleStream(2)

      val rr = RoundRobbin(Array(streamable1, streamable2))

      val stream = rr.getStream

      assertFalse(rr.isInfinite)

      assertEquals(List(1), stream.take(1))
      assertEquals(1, stream.iterator.next)

      compareCallsToGetStream(List(streamable1, streamable2, rr))
  }

}