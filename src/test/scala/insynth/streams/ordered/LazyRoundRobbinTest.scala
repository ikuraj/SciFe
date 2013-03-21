package insynth.streams.ordered

import scala.util.Random

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.{ Test, Ignore }

class LazyRoundRobbinTest extends JUnitSuite {

  val rnd = new Random(System.currentTimeMillis())

  import Utils._

  @Test
  def testLazyRoundRobbinLoop {
    val stream1 = getSingleStream(Stream(1, 2), false)

    val rr = LazyRoundRobbin(List(stream1))

    rr addStreamable rr

    rr.initialize

    val stream = rr.getStream

    val rndValue = rnd.nextInt(1000)

    assertEquals(List.fill(rndValue)(1), stream.take(rndValue))
  }

  @Test
  @Ignore("lazy stream at this point needs to evaluate all needed values to compare")
  def testRoundRobinLoopEvaluationNoThrow {
//    val lazyPair = ((throw new RuntimeException), 2)
//    
//    val innerStream = (1, 1) #:: lazyPair #:: Stream[(Int, Int)]()
    
    val stream1 = getSingleStream(0)
    val stream2 = RoundRobbin(Seq(Singleton[Int](throw new RuntimeException)))
    
    val lrr = LazyRoundRobbin(List(stream1))

    lrr addStreamable UnaryStream(lrr, (x: Int) => x + 1 )

    lrr.initialize

    val stream = lrr.getStream

    val rndValue = rnd.nextInt(1000)
//
//    assertEquals(List.fill(rndValue)(1), stream.take(rndValue))
  }

  @Test(expected = classOf[RuntimeException])
  def testRoundRobinLoopEvaluationThrow {
    val stream1 = getSingleStream(1 #:: (throw new RuntimeException) #:: Stream[Int](), false)

    val rr = LazyRoundRobbin(List(stream1))

    rr addStreamable rr

    rr.initialize

    val stream = rr.getStream

    val rndValue = rnd.nextInt(1000)

    assertEquals(List.fill(rndValue)(1), stream.take(rndValue))
  }

  @Test
  def testLazyRoundRobbinNoLoop {
    val streams = List(
      getSingleStream(6),
      getSingleStream(4),
      getSingleStream(12),
      getSingleStream(15),
      getSingleStream(7))

    val rr = LazyRoundRobbin(streams)

    rr.addStreamable(getSingleStream(5))
    rr.addStreamable(getSingleStream(9))

    rr.initialize

    val stream = rr.getStream

    assertEquals(List(4, 5, 6, 7, 9, 12, 15), stream)
  }

  @Test
  def testLazyRoundRobbinNoLoop2 {
    val streams = List(
      getSingleStream(6),
      getSingleStream(4),
      getSingleStream(12),
      getSingleStream(15),
      getSingleStream(7))

    val rr = LazyRoundRobbin(streams)

    rr.initialize

    val stream = rr.getStream

    assertEquals(List(4, 6, 7, 12, 15), stream)
  }

  @Test
  def testLazyRoundRobbinNoLoop3 {
    val streams = List(
      getSingleStream(6),
      getSingleStream(4),
      getSingleStream(12),
      getSingleStream(15),
      getSingleStream(7))

    val rr = LazyRoundRobbin(streams)

    rr addStreamable rr
    rr addStreamable getSingleStream(5)
    rr addStreamable rr

    rr.initialize

    val stream = rr.getStream

    assertEquals(List.fill(6)(4), stream.take(6))
  }

  @Test
  def testLazyRoundRobbinNoLoop4 {
    val streams = List(
      getSingleStream(1),
      getSingleStream(2))

    val rr = LazyRoundRobbin(streams)

    val us = UnaryStream(rr, { (_: Int) + 1 }, Option({ (_: Int) + 1 }))
    val ss2 = getSingleStream(5)
    
    rr addStreamable us
    rr addStreamable ss2

    rr.initialize

    val stream = rr.getStream

    val solutionsList = List(1, 2, 2, 3, 3, 4, 4, 5, 5, 5)
    
    lazy val message = streamToString(rr.getStream)(10)

    assertEquals(message, solutionsList, stream.take(solutionsList.size))
  }
  
  @Test
  def testLazyRoundRobbinNoLoop5 {
    val streams = List(
      getSingleStream(1),
      getSingleStream(5))

    val rr = LazyRoundRobbin(streams)

    val us = UnaryStream(rr, { (_: Int) + 1 }, Option({ (_: Int) + 1 }))
    val ss2 = getSingleStream(2)
    
    rr addStreamable us
    rr addStreamable ss2

    rr.initialize

    val stream = rr.getStream

    val solutionsList = List(1, 2, 2, 3, 3, 4, 4, 5, 5, 5)
    
    lazy val message = streamToString(rr.getStream)(10)

    assertEquals(message, solutionsList, stream.take(solutionsList.size))
  }

}