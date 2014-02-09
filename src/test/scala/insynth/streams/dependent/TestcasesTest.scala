package insynth
package streams
package dependent

import reconstruction.stream.Application
import reconstruction.stream._

import org.scalatest._
import org.scalatest.matchers._

import util._
import util.format._
import common._

class TestcasesTest extends FunSuite with ShouldMatchers {

  test("BST") {
    val ranges = Producer.finite(
      { (v: Range) => v.toStream }
    )
    val sizes = Producer.finite(
      { (s: Int) => (0 to s).toStream }
    )
    val 
    
    
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