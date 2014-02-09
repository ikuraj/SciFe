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

class BinaryTest extends FunSuite with ShouldMatchers {

  test("sorted lists") {
    
    val maxLength = 10
    
    val intProducer = Producer(
      { (v: Int) => Stream.from(v, -1) }
    )
    val listChooser = Producer(
       { (v: Int) => Stream.from(v).map( el => List(el to maxLength: _*) ) }
    )
    val binaryStream = Binary(intProducer, listChooser) ( _ + 1 ) (
      (v: Int, l: List[Int]) => v :: l
    )
    
    withClue (binaryStream.getStream(5).take(5).mkString(", ")) {
      binaryStream.getStream(0) should be ( Nil )
      binaryStream.getStream(1) should be ( Stream(List(1)) )
      binaryStream.getStream(2).map(_.reverse) should be ( Stream(List(1, 2)) )
      binaryStream.getStream(3).map(_.reverse) should be ( Stream(List(1, 2, 3)) )
    }
  }
  
}