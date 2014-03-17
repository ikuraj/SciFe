package insynth.streams
package light

import org.scalatest._

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class RoundRobbinTest extends FunSuite {
  
  import Utils._
  
  test("Simple accesses, equal") {
    val rr = RoundRobbinFinite.equal(Array(
      WrapperArray(1, 2, 3),
      WrapperArray(4, 5, 6),
      WrapperArray(7, 8, 9)
    ))
    
    (0 until 9).map(
      rr(_)
    ) should be ( List(1, 4, 7, 2, 5, 8, 3, 6, 9) )
    
  }

  test("Simple accesses, fixed") {
    val arrays: Array[Enum[Int]] = Array(
      WrapperArray(1, 2, 3),
      WrapperArray(4, 5, 6),
      WrapperArray(7, 8, 9)
    )
    arrays.size should be (3)
    
    val rr = RoundRobbinFinite.fixed[Int](arrays)

    for (target <- 0 until 9) {
      rr.binarySearch(target) should be (target / 3)
    }
    
    (0 until 9).map(
      rr.apply(_)
    ) should be ( 1 to 9 )
    
  }

  test("Simple accesses, fixed, one-element Enums") {
    val arrays: Array[Enum[Int]] = Array(
      WrapperArray(1),
      WrapperArray(4),
      WrapperArray(7)
    )
    arrays.size should be (3)
    
    val rr = RoundRobbinFinite.fixed[Int](arrays)

    rr.size should be (3)
    for (target <- 0 until 3) {
      rr.binarySearch(target) should be (target)
    }
    
//    (0 until 3).map(
//      rr.apply(_)
//    ) should be ( List(1, 4, 7) )
    
  }
  
  test("Simple accesses and appending, buffer") {
    val rr = RoundRobbinFinite.buffer(Array(
      WrapperArray(1 to 30),
      WrapperArray(31 to 60),
      WrapperArray(61 to 90)
    ))
    
    for (target <- 0 to 89) {
      rr.binarySearch(target) should be (target / 30)
      rr(target) should be (target + 1)
    }
    
    rr.append(WrapperArray(91 to 120))
    
    for (target <- 0 to 119) {
      rr.binarySearch(target) should be (target / 30)
      rr(target) should be (target + 1)
    }
    
  }
  
  test("RoundRobbin with empty array") {
    val rr = RoundRobbinFinite.fixed[Int](Array(
    ))
    
    rr.size should be (0)
  }
  
}