package scife.enumeration
package member
package memoization

import scife.{ enumeration => e }
import dependent._

import util._
import scife.util.logging._
import scife.util._

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class MemoizedMemberTest extends FunSuite with Matchers
  with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {  
  import Checks._
  import structures._
  import BSTrees._
  
  test("memoized enumeration") {
    val memoizedEnum =
      new {
        override val classTagT = implicitly[scala.reflect.ClassTag[(Int, Int)]] 
      } with member.ProductFinite( 
        new WrapArray( Array(1 to 10: _*) ),
        new WrapArray( Array(10 to 1 by -1: _*) )
      ) with e.memoization.MemoizedSize with e.memoization.MemoizedStatic[(Int, Int)]
    with Memoized[(Int, Int)]

    val enumerated =
      for ( ind <- 0 until memoizedEnum.size) yield {
        memoizedEnum(ind)
      }

    for ( ind <- 0 until memoizedEnum.size) {
      memoizedEnum.member(enumerated(ind)) shouldBe true
      memoizedEnum.members should contain (enumerated(ind))
    }
  }
  
  test("memoized member") {
    val memoizedEnum =
      new {
        override val classTagT = implicitly[scala.reflect.ClassTag[(Int, Int)]] 
      } with member.ProductFinite( 
        new WrapArray( Array(1 to 10: _*) ),
        new WrapArray( Array(10 to 1 by -1: _*) )
      ) with e.memoization.MemoizedSize with e.memoization.MemoizedStatic[(Int, Int)]
    with Memoized[(Int, Int)]

    val enumerated =
      for ( ind <- 0 until memoizedEnum.size) yield {
        memoizedEnum(ind)
      }

    for ( ind <- 0 until memoizedEnum.size) {
      memoizedEnum.members should contain (enumerated(ind))
      memoizedEnum.member(enumerated(ind)) shouldBe true
    }
  }  
  
}
