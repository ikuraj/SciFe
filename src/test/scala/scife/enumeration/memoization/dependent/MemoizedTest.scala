package scife
package enumeration
package memoization
package dependent

import enumeration.dependent._

import org.scalatest._

class MemoizedTest extends FunSuite with Matchers {

  def makeTdenum = new WrapFunction( { (self: Depend[Int, Int], i: Int) =>
    if (i == 0) Enum(1, 2, 3)
    else ((Enum(i): Enum[Int]) ++ (self(i-1): Enum[Int])): Enum[Int]
  } ) with Memoized[Int, Int]
  
  test("memoizing given invocations") {
    val tdenum = makeTdenum
    
    for (i <- List(1, 10, 15)) {
      tdenum(i)
      
      tdenum.memoizedMap contains i shouldBe true
    }
  }

  test("memoizing smaller recursive invocations") {
    val tdenum = makeTdenum
    
    for (i <- List(1, 10, 15)) {
      tdenum(i)
      
      for (j <- 1 to i)
        tdenum.memoizedMap contains j shouldBe true
    }
    
  }
  
}
