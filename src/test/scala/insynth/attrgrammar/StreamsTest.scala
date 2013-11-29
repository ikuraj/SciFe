package insynth
package attrgrammar

import org.scalatest._

import common._
import streams._
import streams.{ ordered => ord }
import reconstruction.stream._
import util.format._

class StreamsTest extends FunSuite {

  val streamFactory = new OrderedStreamFactory[Any]
  
  import streamFactory._
  import ord._
  
  val nilStream = makeSingletonList(Nil)
  val genStream = makeFiniteStream( Vector(1, 2, 3) )
//    makeSingleton(1)
  
  val listStream = makeLazyRoundRobbinList(List(nilStream))

  val constructorStream =
    makeBinaryStream(listStream, genStream) { (list, el2) => list :+ el2 }
  
  listStream addStreamable constructorStream
  listStream.initialize

  val streamable = listStream
  val stream =
    streamable match {
      case os: OrderedStreamable[_] =>
        fine("returning ordered streamable")
        os.getStream zip os.getValues.map(_.toFloat)
      case us: Streamable[_] =>
        fine("returning unordered streamable")
        us.getStream zip Stream.continually(0f)
    }
  
  assert( stream.take(30) contains (List(1, 2), 4.0) )
  assert( stream.take(80) contains (List(2, 2, 1), 6.0) )

}
