package insynth.enumeration
package member
package testcase

import insynth.{ enumeration => e }
import dependent._

import util._
import insynth.util.logging._
import insynth.util._

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

class SortedListTest extends FunSuite with Matchers with PropertyChecks
  with HasLogger with ProfileLogger {
  import Checks._
  import structures._
  import Util._

  test("regular enumeration") {
    val lists = constructMemberElaborate
    
    {
      val en = lists.getEnum(1, 2)
      val elements = for (ind <- 0 until en.size) yield en(ind)
      en.size should be(2)
    }

    {
      val en = lists.getEnum(2, 2)
      val elements = for (ind <- 0 until en.size) yield en(ind)
      en.size should be(3)
    }

    {
      val en = lists.getEnum(2, 3)
      val elements = for (ind <- 0 until en.size) yield en(ind)
      en.size should be(6)
    }

    checkInvariant(lists)
  }

  test("member enumeration") {
    val lists = constructMemberElaborate
  
    lists( 3, 3 ).member(List(1, 1, 1)) should be (true)  
    lists( 3, 3 ).member(List(3, 3, 3)) should be (true) 
    lists( 3, 3 ).member(List(3, 2, 2)) should be (true) 
    lists( 3, 3 ).member(List(3, 3, 1)) should be (true)
    lists( 3, 3 ).member(List(3, 3, 2)) should be (true)  
    lists( 3, 3 ).member(List(3, 2, 1)) should be (true)
    
    lists( 3, 3 ).member(List(1, 2, 3)) should be (false)

    {
      val en = lists.getEnum(1, 2): Member[List[Int]]
      for (list <- List(List(1), List(2))) {
        en.member(list) should be (true)
      }
    }

    val normalLists = constructEnumeratorNormal
    
    forAll(Gen.choose(1, 5), Gen.choose(1, 5), minSuccessful(20)) {
      (size: Int, m: Int) =>
      {
        val normalList = normalLists.getEnum( size, m )
        
        for ( ind <- 0 until normalList.size) {
          lists( size, m ).member( normalList(ind) ) should be (true)
        }
        
      }
    }
  }

  def checkInvariant(lists: e.dependent.Depend[(Int, Int), List[Int]]) =
    // keep this simple - we are not using memoization in tests
    forAll(Gen.choose(1, 5), Gen.choose(1, 5), maxSize(50)) { (size: Int, m: Int) =>
      //      finest("size =%d, m=%d".format(size: Int, m: Int))
      whenever(true) {
        val elements = lists.getEnum( (size, m) ).toList

        for (lst <- elements; (i, j) <- lst.zip(lst.tail)) {
          assert(i >= j)
        }
      }
    }

  def constructMemberElaborate: MemberDependFinite[(Int, Int), List[Int]] = {

    val naturals = new WrapFunctionFin( (range: Int) =>
      { new WrapArray( Array(1 to range: _*) ) }
    )

    new WrapFunctionFin(
      (self: MemberDependFinite[(Int, Int), List[Int]], pair: (Int, Int)) => {
        val (size, max) = pair

        if (size == 0) new WrapArray( Array(List.empty[Int]) ): MemberFinite[List[Int]]
        else if (size > 0) {
          val roots: MemberFinite[Int] = naturals(max)

          val innerLists: MemberDependFinite[Int, List[Int]] = new InMapFin(
            self, { (par: Int) => (size - 1, par) }
          )

          val allLists: MemberFinite[(Int, List[Int])] =
            new ChainFinite(roots, innerLists)

          val makeList =
            (p: (Int, List[Int])) => {
              p._1 :: p._2
            }

          val memberList =
            (list: List[Int]) => {
              (list.head, list.tail)
            }

          new Map[(Int, List[Int]), List[Int]](allLists, makeList, memberList)
            with MemberFinite[List[Int]]
            : MemberFinite[List[Int]]
        } else new WrapArray( Array( ) ): MemberFinite[List[Int]]
      })
  }
  
  def constructEnumeratorNormal = {
    import e.dependent.{ Depend, InMap }
    type EnumType = Depend[(Int, Int), List[Int]]
    
    val naturals = Depend((range: Int) => { e.WrapArray( 1 to range ) })
    
    Depend.memoized(
      ( self: EnumType, pair: (Int, Int) ) => {
        val (size, max) = pair

        if (size == 0) e.Singleton( Nil )
        else if (size > 0) {
          val roots = naturals.getEnum(max)
          
          val innerLists: Depend[Int, List[Int]] = InMap(self, { (par: Int) =>
            (size - 1, par)
          })
          
          val allLists =
            e.dependent.Chain[Int, List[Int], List[Int]](roots, innerLists,
              (head: Int, l: List[Int]) => {
                head :: l
              }
            )
          
          allLists
        } else e.Empty
      }
    )
  }
  
}
