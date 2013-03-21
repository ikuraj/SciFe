package insynth.streams.ordered

import scala.util.Random

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.Test

class StreamCombinationsTest extends JUnitSuite {

  val rnd = new Random(System.currentTimeMillis())

  import Utils._

  @Test
  def testBinaryStreamLoop {
    val stream1 = getSingleStream(1)

    val rr = LazyRoundRobbin(List(stream1))

    val stream2 = getSingleStream(Stream(3, 4), false)

    val bs = BinaryStream(rr, stream2) {
      (x, y) => x * y
    }

    rr addStreamable bs

    rr.initialize

    val streamString = streamToString(rr.getStream zip rr.getValues)(10)

    val valResult = List(1, 4, 5, 7, 8, 8, 9)

    assertEquals(streamString, valResult, rr.getValues.take(valResult.size))

    val elResult = List(1, 3, 4, 9, 12, 12, 16, 27)

    assertEquals(streamString, elResult, rr.getStream.take(elResult.size))
  }

  @Test
  def testBinaryStreamLoop2 {
    val stream1 = getSingleStream(1)

    val rr = LazyRoundRobbin(List(stream1))

    val stream2 = getSingleStream(Stream(3, 4), false)

    val bs = BinaryStream(stream2, rr) {
      (x, y) => x * y
    }

    rr addStreamable bs

    rr.initialize

    val streamString = streamToString(rr.getStream zip rr.getValues)(10)

    val valResult = List(1, 4, 5, 7, 8, 8, 9)

    assertEquals(streamString, valResult, rr.getValues.take(valResult.size))

    val elResult = List(1, 3, 4, 9, 12, 12, 16, 27)

    assertEquals(streamString, elResult, rr.getStream.take(elResult.size))
  }

  @Test(expected = classOf[RuntimeException])
  def testBinaryStreamLoopEvaluationThrow {
    val stream1 = getSingleStream(1 #:: (throw new RuntimeException) #:: Stream.empty[Int], false)

    val rr = LazyRoundRobbin(List(stream1))

    val stream2 = getSingleStream(Stream(3, 4), false)

    val bs = BinaryStream(stream2, rr) {
      (x, y) => x * y
    }

    rr addStreamable bs

    rr.initialize

    rr.getValues.take(6)    
    val streamString = streamToString(rr.getStream zip rr.getValues)(6)
    assertTrue("Stream string: " + streamString, List.empty != (rr.getStream zip rr.getValues take 6))
  }

  @Test
  def testBinaryStreamLoopEvaluationNoThrow {
    val stream1 = getSingleStream(1 #:: (throw new RuntimeException) #:: Stream.empty[Int], true)

    val rr = LazyRoundRobbin(List(stream1))

    val stream2 = getSingleStream(Stream(1, 4), false)

    val bs = BinaryStream(stream2, rr) {
      (x, y) => x * y
    }

    rr addStreamable bs

    rr.initialize

    val randomVal = rnd.nextInt(1000)

    rr.getValues.take(randomVal)
  }

  @Test
  def testWithBinaryStream1 {

    val streamable1 = getSingleStream(1)
    val streamable2 = getSingleStream(2)

    val streamable3 = LazyRoundRobbin(List(streamable1))
    val streamable4 = LazyRoundRobbin(List(streamable2))

    streamable3.initialize
    streamable4.initialize

    {
      val bs = BinaryStream(streamable3, streamable4) {
        (x, y) => x + y
      }

      val stream = bs.getStream

      assertFalse(bs.isInfinite)
      assertEquals(
        stream.take(1) mkString (", "),
        List(3),
        stream.take(1).toList)
    }
  }

  @Test
  def testWithBinaryStream2 {
    val streamable1 = getSingleStream(Stream(1, 3, 7), false)
    val streamable2 = getSingleStream(Stream(2, 3, 6), false)

    val streamable3 = LazyRoundRobbin(List(streamable1))
    val streamable4 = LazyRoundRobbin(List(streamable2))

    {
      val bs = BinaryStream(streamable3, streamable4) {
        (x, y) => List(x, y)
      }

      streamable3.initialize
      streamable4.initialize

      val stream = bs.getStream

      assertFalse(bs.isInfinite)
      assertEquals(
        stream.take(5) mkString (", "),
        List(List(1,2), List(1,3), List(3,2), List(3,3), List(1,6)),
        stream.take(5))
    }
  }

  @Test
  def testBinaryStreamLoop3 {
    val stream1 = getSingleStream(1)

    val rr = LazyRoundRobbin(List(stream1))

    val stream2 = getSingleStream(Stream(2), false)
    val stream3 = getSingleStream(Stream(3), false)

    val bs1 = BinaryStream(stream2, rr) {
      (x, y) => x * y
    }

    val bs2 = BinaryStream(stream3, rr) {
      (x, y) => x * y
    }

    rr.streams ++= List(bs1, bs2)

    rr.initialize

    val streamString = streamToString(rr.getStream zip rr.getValues)(10)

    val valResult = List(1, 3, 4, 5, 6)

    assertEquals(streamString, valResult, rr.getValues.take(valResult.size))

    val elResult = List(1, 2, 3, 4, 6)

    assertEquals(streamString, elResult, rr.getStream.take(elResult.size))
  }

  @Test
  def testBinaryStreamLoop5 {
    val stream1 = getSingleStream(0)

    val rr = LazyRoundRobbin(List(stream1))

    val stream2 = getSingleStream(Stream(3), false)
    val stream3 = getSingleStream(Stream(5), false)

    val bs1 = BinaryStream(stream2, rr) {
      (x, y) => x + y
    }

    val bs2 = BinaryStream(stream3, rr) {
      (x, y) => x + y
    }

    rr.streams ++= List(bs1, bs2)

    rr.initialize

    val stream = rr.getStream

    val randomValue = rnd.nextInt(1000)

    lazy val streamString = streamToString(rr.getStream)(50)

    val computedSeq =
      for (i <- 0 to randomValue; j <- 0 to randomValue)
        yield i * 3 + j * 5

    val streamList = stream.take(randomValue).distinct
        
    val computedList = computedSeq.sortWith(_ < _).take(randomValue).distinct.take(streamList.size).toList

    assertEquals(streamString, computedList.size, streamList.size)
    assertEquals("Computed: " + computedList.mkString(",") + "\nStream: " + streamToString(stream)(50), computedList, streamList)
  }

}