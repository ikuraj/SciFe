package scife
package enumeration
package iterable
package lzy
package testcase

import scife.{ enumeration => e }
import e.iterable._
import e.dependent._

import scife.util.logging._
import scife.util._

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

import scala.language.postfixOps

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class SortedListTest extends FunSuite with Matchers
  with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {
  
  import common._
  import enumdef._
  import scife.util.structures._
  
  type EnumType[A] = LazyEnumFinite[A]/* with Memoized[A]*/
  type DepEnumType[I, O] = Depend[I, O] { type EnumSort[A] = SortedListTest.this.EnumType[A] }
  type DepEnumTypeFinite[I, O] = DependFinite[I, O] { type EnumSort[A] = SortedListTest.this.EnumType[A] }
 
  implicit val ms = e.memoization.scope.NoScope 

  test("correctness of enumeration") {
    val enum = constructEnumerator
    val base = SortedListEnum.constructEnumeratorStrict

//    forAll( Gen.choose(0, 10), Gen.choose(0, 10), minSuccessful(10) ) {
//      case (size, range) =>
//        whenever (range > size) {
    for (size <- 0 to 7; range <- 0 to 7; if range > size) {
      enum(size, range).size shouldBe base(size, range).size
      for(i <- 0 until enum.size)
        enum(size, range)(i) shouldBe base(size, range)(i)
    }

  }

  ignore("correctness of iterative enumeration") {
    val enum = constructEnumerator
    val base = SortedListEnum.constructEnumeratorStrict

    for (size <- 0 to 7; range <- 0 to 7; if range > size) {
      val itEnum = enum(size, range)
      val baseEnum = base(size, range)
      for(i <- 0 until baseEnum.size)
        itEnum.next shouldBe baseEnum(i)
    }

  }

  ignore("correctness of iterative enumeration, hasNext") {
    val enum = constructEnumerator
    val base = SortedListEnum.constructEnumeratorStrict

    for (size <- 0 to 7; range <- 0 to 7; if range > size) {
      val itEnum = enum(size, range)
      val baseEnum = base(size, range)
      for(i <- 0 until baseEnum.size)
        itEnum.next shouldBe baseEnum(i)

      itEnum.hasNext shouldBe false
    }

  }
  
  test("resetting enumerator in the middle") {
    val depEnum = constructEnumerator
    val enum = depEnum(7, 7)

    val halfOfTheEnum =
      for (ind <- 0 until enum.size/2)
        yield enum(ind)
        
    enum.reset
    
    for ((el, ind) <- halfOfTheEnum.zipWithIndex)
      enum(ind) should be (el)

  } 
  
  def constructEnumerator(implicit ms: e.memoization.MemoizationScope) = {

    new WrapFunctionTest[(Int, Int), List[Int], EnumType](
      ( self: DepEnumType[(Int, Int), List[Int]], pair: (Int, Int) ) => {
        val (size, max) = pair

        if (size == 0) (new e.Singleton( Nil ) with ResetIter[List[Int]] with
          Touchable[List[Int]]): EnumType[List[Int]]
        else //if (size > 0)
          {
            val roots = new e.WrapArray( 1 to max toArray ) with ResetIter[Int] with Touchable[Int]
  
            val innerLists: DepEnumType[Int, List[Int]] = new InMap[(Int, Int), Int, List[Int],
              DepEnumType] (self, { (par: Int) =>
              (size - 1, par - 1)
            }) with DependFinite[Int, List[Int]] {
              override type EnumSort[A] = SortedListTest.this.EnumType[A]
            }
  
            val allLists =
              new iterable.lzy.dependent.ChainFiniteSingleCombine[Int, List[Int], List[Int]](roots,
                innerLists.asInstanceOf[DepEnumTypeFinite[Int, List[Int]]],
                (head: Int, l: List[Int]) => {
                  head :: l
                }
              )
  
            allLists: LazyEnumFinite[List[Int]]
        }
      }
    )
  }

}
