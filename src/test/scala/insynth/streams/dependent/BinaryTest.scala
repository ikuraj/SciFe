package insynth
package streams
package dependent

import org.scalatest._

import streams.{ light => e }

import util._
import util.format._
import common._

class BinaryTest extends FunSuite with Matchers {
  
  test("empty binary") {
    
    val emptyProducer = Producer(
      { (v: Int) => e.Empty }
    )
    
    val array = e.WrapperArray( 1 to 100 )
    
    
    val binary = BinaryFinite.apply(array, emptyProducer)
    
    binary.size should be (0)
    
  }

//  test("sorted lists") {
//    
//    val maxLength = 10
//    
//    val intProducer = Producer.finite(
//      { (v: Int) => e.Singleton(v) }
//    )
//    
//    var getList: Int => e.Finite[List[Int]] = null
//    
//    val listChooser: FiniteDependent[Int, List[Int]] = Producer.finite(
//       { (v: Int) =>
//   			v match {
//   			  case 0 => e.Singleton(List[Int]())
//   			  case _ => e.Binary( e.Singleton(v), getList(v - 1) )( _ :: _ )
//   			}
//       }
//    )
//    
//    getList = (v: Int) => listChooser.getStream(v)
//    
//    val enum = listChooser.getStream(5)
//    
//    enum.size should be (1)
//  }

}