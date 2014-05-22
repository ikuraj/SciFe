package insynth.enumeration
package testcases

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._

import dependent._
import insynth.{ enumeration => e }
import memoization._

import insynth.util._
import insynth.util.logging._
import Structures.BSTrees._

import org.scalatest._
import org.scalameter.api._

import scala.language.postfixOps
import scala.language.existentials

class DAGTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
	HasLogger with ProfileLogger { 
  import Checks._
  import Structures._
  import BSTrees._
  import Util._
  
  // (size, available, declared)
  type Input = (Int, Set[Int], Set[Int])
  // list of extends
  type Output = List[(Int, List[Int])]
  type EnumType = Depend[Input, Output]

  test("enumeration") {
    val checkerHelper = new CheckerHelper[Output]
    import checkerHelper._
    
    def rangeList(m: Int) = m to 0 by -1 toArray
    val enum = constructEnumerator

    withLazyClue("Elements are: " + clue) {
      // (size, available, declared)
      
      res = enum.getEnum((1, Set(1), Set()))
      res.size should be (1)

      res = enum.getEnum((2, Set(1, 2), Set()))
      res.size should be (2)

      res = enum.getEnum((2, Set(1, 2), Set(1)))
      res.size should be (2)

      res = enum.getEnum((3, 1 to 3 toSet, Set()))
      res.size should be (8)

      res = enum.getEnum((3, 1 to 3 toSet, Set()))
      // class or interface
      res.size should be (8)

      res = enum.getEnum((5, 1 to 5 toSet, Set()))
      // class or interface
      res.size should be (1024)
      
//      res = enum.getEnum((1, Set(1), Set(2)))
//      res.size should be (2)
//      
//      res = enum.getEnum((1, Set(3), Set(4, 5)))
//      res.size should be (4)

//      res = enum.getEnum((4, 1 to 4 toSet, Set()))
//      res.size should be (95)
//
//      res = enum.getEnum((5, 1 to 5 toSet, Set()))
//      res.size should be (4858)

      res = enum.getEnum((1, Set(1), Set()))
      res.size should be (1)

      res = enum.getEnum((1, Set(1), Set()))
      res.size should be (1)

      res = enum.getEnum((1, Set(1), Set()))
      res.size should be (1)

      res = enum.getEnum((1, Set(1), Set()))
      res.size should be (1)

      res = enum.getEnum((1, Set(1), Set()))
      res.size should be (1)
    }
      
  }
  
  test("subListChooser") {
    val checkerHelper = new CheckerHelper[List[Int]]
    import checkerHelper._
    
    def rangeList(m: Int) = m to 0 by -1 toArray
    val enum = constructEnumerator

    withLazyClue("Elements are: " + clue) {
      for(s <- 1 to 5; m <- 1 to s) {
        addMessage = "m=%d and s=%d".format(m, s)
        res = subListChooser.getEnum( (m, 1 to s toList) )
        val listCombinations: List[List[Int]] =
          ((1 to s toList) combinations m) toList
  
        res.size should be (listCombinations.size)
        elements should contain theSameElementsAs (listCombinations)
      }
      
    }
  }

  val subListChooser: DependFinite[(Int, List[Int]), List[Int]] = Depend.memoizedFin(
    (self: DependFinite[(Int, List[Int]), List[Int]], pair: (Int, List[Int])) => {
      val (size, range) = pair

      if (size <= 0) e.Singleton(Nil): Finite[List[Int]]
      else if (size == 1) e.Enum(range map {List(_)}): Finite[List[Int]]
      else if (size <= range.size) {
        val temp = self.getEnum( (size - 1, range.tail) )
        val kept = Map( temp , { range.head :: (_: List[Int]) })
        val leftOut = self.getEnum( (size, range.tail) )
        
        val allNodes = e.Concat(kept, leftOut)
        allNodes: Finite[List[Int]]
      } else e.Empty: Finite[List[Int]]
    })

//  val sublistForSizesUpTo = Depend.memoizedFin( (p: (Int, Set[Int]) ) => {
//    val (classes: Int, available: Set[Int]) = p
//    val chain = memoization.Chain(
//      (e.Map(e.Enum(1 to classes), { (el: Int) => (el, available.toList ) } )): Finite[(Int, List[Int])],
//      subListChooser: DependFinite[(Int, List[Int]), List[Int]]
//    ): Finite[((Int, List[Int]), List[Int])]
//    
//    if (classes == 0) e.Singleton(Nil): Finite[List[Int]]
//    else //e.Concat(
//      Map(chain, { (r: ((Int, List[Int]), List[Int])) => r._2 })
//
//    }
//  )
      
  def constructEnumerator(implicit ms: MemoizationScope = null) = {
    
    Depend.memoizedFin[Input, Output](
      (self: DependFinite[Input, Output], par: Input) => {
      // list sorted descendingly
      implicit val (size, available, declared) = par
      
      def declaredOK(i: Int) =
        declared.toList.filter( _ > i )
//        declared.toList
        
      def declareRange(i :Int) = 0 to declaredOK(i).size

      val adapted = Depend.fin( (myId: Int) => {
	        memoization.Chain.fin(
	          e.WrapArray( declareRange(myId) map { (_, declaredOK( myId )) } ): Finite[(Int, List[Int])],
	          subListChooser
          ) map {
	          (p: ((Int, List[Int]), List[Int])) => p._2
	        }
        }
      )
      
        //InMap(sublistForSizesUpTo, { (i: Int) => (i, declared) } )

//      assert(size == available.size)
      if (size > available.size) e.Empty
      else
      if (size == 1) {
        
        adapted(available.head) map {
          (l: (List[Int]) ) =>
          { List( (available.head, l) ) }
        } : Finite[Output]
      }
      else {
        val rest: DependFinite[Int, Output] =
          InMap(self, { (par: Int) =>
            //(size - 1, available - par, declared + par)
            (size - 1, available.filter( _ < par), declared + par)
          })
          
//        e.Product(
          memoization.Chain.fin[Int, (Output, List[Int])](
            e.WrapArray(available.toArray): Finite[Int],
            e.dependent.Product(rest, adapted)
          ) map { (r: (Int, (Output, List[Int]))) =>
          val (chosen, (rest, extended)) = r
          
          (chosen, extended) :: rest
        } : Finite[Output]
      }
    })
  }
  
}
