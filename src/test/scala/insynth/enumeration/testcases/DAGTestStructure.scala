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

class DAGTestStructure extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
	HasLogger with ProfileLogger { 
  import Checks._
  import Structures._
  import BSTrees._
  import Util._
  
  // (size, #class)
  type Input = (Int, Int)
  // list of extends
  type Output = List[List[Int]]
  type EnumType = Depend[Input, Output]

  test("enumeration") {
    val checkerHelper = new CheckerHelper[Output]
    import checkerHelper._
    
    def rangeList(m: Int) = m to 0 by -1 toArray
    val enum = constructEnumerator

    withLazyClue("Elements are: " + clue) {
        
      // (size, #class, #interface, #overridableMethods)
      res = enum.getEnum((1, 0))
      // class or interface
      res.size should be (1)

      res = enum.getEnum((2, 0))
      // class or interface
      res.size should be (2)

      res = enum.getEnum((3, 0))
      // class or interface
      res.size should be (8)

      res = enum.getEnum((5, 0))
      // class or interface
      res.size should be (1024)

//      res = enum.getEnum((4, 0))
//      // class or interface
//      res.size should be (95)
      
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

  val sublistForSizesUpTo: DependFinite[Int, List[Int]] = Depend.memoizedFin((classes: Int) => {
    val chain = memoization.Chain(
      (e.Map(e.Enum(1 to classes), { (el: Int) => (el, (1 to classes).toList ) } )): Finite[(Int, List[Int])],
      subListChooser: DependFinite[(Int, List[Int]), List[Int]]
    ): Finite[((Int, List[Int]), List[Int])]
    
    e.Concat(
      e.Singleton(Nil): Finite[List[Int]],
      Map(chain, { (r: ((Int, List[Int]), List[Int])) => r._2 })
    )}
  )
      
  def constructEnumerator(implicit ms: MemoizationScope = null) = {
    
    Depend.memoized(
      (self: EnumType, par: Input) => {
      // list sorted descendingly
      implicit val (size, classes) = par

      if (size == 1) {
        sublistForSizesUpTo(classes) map { List(_) }
      }
      else {
        e.Product(
          sublistForSizesUpTo(classes),
          self(size - 1, classes + 1)
        ) map { case (h, tail) => h :: tail }
      }
    })
  }
  
}
