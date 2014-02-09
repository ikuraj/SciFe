package insynth
package streams
package dependent

import org.scalatest._
import org.scalatest.matchers._

import reconstruction.stream.Application
import reconstruction.stream._

import util._
import util.format._
import common._

class StreamCombinationTest extends FunSuite with ShouldMatchers {

  test("sorted lists") {
    val intProducer = Producer.finite(
      { (v: Int) =>
        if (v > 0) Stream(v)
        else Stream.empty
      }
    )
    val listChooser = Producer.finite[Int, List[Int]](null)
    val binaryStream = FiniteBinary(intProducer, listChooser) ( _ - 1 ) (
      (v: Int, l: List[Int]) => v :: l
    )
    listChooser.producerFunction = {
      (v: Int) => if (v > 0) binaryStream.getStream(v) else Stream(Nil)
    }
    
    binaryStream.getStream(0) should be ( Nil )
    binaryStream.getStream(1) should be ( Stream(List(1)) )
    binaryStream.getStream(2).map(_.reverse) should be ( Stream(List(1, 2)) )
    binaryStream.getStream(3).map(_.reverse) should be ( Stream(List(1, 2, 3)) )
  }
  
}