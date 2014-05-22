package insynth.enumeration
package testcases

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._

import insynth.{ enumeration => e }
import e.dependent._

import util._
import insynth.util.logging._
import insynth.util._
  
import scala.language.existentials
import scala.language.postfixOps

class HeapArrayTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
	HasLogger with ProfileLogger {  
  import e._
  
  import Checks._
  import Structures._
  import BSTrees._
  import Util._
  
  test("sublists enumeration") {
    val checkerHelper = new CheckerHelper[List[Int]]
    import checkerHelper._

    val setChooser = sublistsEnum

    withLazyClue("Elements are: " + clue) {
	    for (m <- 1 to 10) {
      	res = setChooser.getEnum( (0, 1 to m toList) )
      	res.size should be (0)
	    }

	    for (m <- 2 to 10) {
      	res = setChooser.getEnum( (1, 1 to m toList) )
//      	res shouldBe a [Map[_,_]]
//      	res shouldBe a [WrapArray[_]]
      	res.size should be (m)
      	elements should contain theSameElementsAs( 1 to m map { List(_) } )
	    }

//	    forAll ("size", "range max") { (s: Int, m: Int) =>
//	      whenever (m > 0 && s < 10 && m < s) {
	    	for(s <- 1 to 10; m <- 1 to s) {
	        addMessage = "m=%d and s=%d".format(m, s)
	        res = setChooser.getEnum( (m, 1 to s toList) )
	        val listCombinations: List[List[Int]] =
	          ((1 to s toList) combinations m) toList

	        res.size should be (listCombinations.size)
	        elements should contain theSameElementsAs (listCombinations)
    		}
//	    }
    }
  }
  
  test("heap enumeration") {
    val checkerHelper = new CheckerHelper[Tree]
    import checkerHelper._
    
    def rangeList(m: Int) = m to 1 by -1 toList
    val enum = heapsEnum

    withLazyClue("Elements are: " + clue) {
	    for (m <- 1 to 10) {
      	res = enum.getEnum( (0, rangeList(m: Int)) )
      	res.size should be (1)
      	elements should contain only (Leaf)
	    }

	    for (m <- 2 to 10) {
      	res = enum.getEnum( (1, rangeList(m: Int)) )
//      	res shouldBe a [Map[_, _]]
//      	res shouldBe a [WrapArray[_]]
      	res.size should be (m)
	    }

//	    forAll ("size", "range max") { (s: Int, m: Int) =>
//	      whenever (m > 0 && s < 10 && m < s) {
	    	for(s <- 1 to 10; m <- 1 to s) {
	        addMessage = "m=%d and s=%d".format(m, s)
	        res = enum.getEnum( (m, rangeList(m: Int)) )

//	        res.size should be (listCombinations.size)
	        elements.forall( heapProperty(_) ) should be (true)
    		}
//	    }
	    	
      addMessage = ""

    	res = enum.getEnum( (3, rangeList(3)) )
    	res.size should be (2)
    }
  }

  def sublistsEnum = {
    
    val listChooser = Depend.memoized(
      (self: Depend[(Int, List[Int]), List[Int]], pair: (Int, List[Int])) => {
        val (size, list) = pair

        if (size <= 0) e.Empty: e.Enum[List[Int]]
        else
        if (size == 1) e.Enum((list map { List(_) }).toList): e.Enum[List[Int]]
        else if (size <= list.size) {
          val kept = self.getEnum( (size - 1, list.tail) ) map { list.head :: _ }
          val leftOut = self.getEnum( (size, list.tail) )
          
          val allNodes = e.Concat(kept, leftOut)
          allNodes
        } else e.Empty
      })
      
    listChooser
  }
  
  def heapsEnum = {
    Depend.memoized(
      (self: Depend[(Int, List[Int]), Tree], pair: (Int, List[Int])) => {
        // list sorted descendingly
        val (size, list) = pair

        if (size <= 0) e.Singleton(Leaf)
        else if (size == 1)
//          e.Enum(list map { v => Node(Leaf, v, Leaf) }): Finite[Node]
//          e.WrapArray( (list map { v => Node(Leaf, v, Leaf) }).toArray ): Finite[Tree]
          //(e.Enum(list): Finite[Int]) map { v => Node(Leaf, v, Leaf) }
          Map(e.Enum(list), { (v: Int) => Node(Leaf, v, Leaf) } ): Finite[Tree]
        else if (size <= list.size) {
          val root = list.head
          val sublists = sublistsEnum.getEnum( ((size-1)/2, list.tail) )

          val leftHeaps = InMap(self, { (left: List[Int]) =>
            ( (size-1)/2, left )
          })
          val rightHeaps = InMap(self, { (left: List[Int]) =>
            ( (size-1)/2, list.tail.diff(left) )
          })
          val leftRightPairs: Depend[List[Int], (Tree, Tree)] =
            e.dependent.Product(leftHeaps, rightHeaps)
          
          val allNodes =
            memoization.Chain[List[Int], (Tree, Tree), Node](sublists, leftRightPairs,
              (leftElements: List[Int], p2: (Tree, Tree)) => {
                val (leftTree, rightTree) = p2

                Node(leftTree, root, rightTree)
              })

          allNodes
        } else e.Empty
      })
  }
  
//  def subsetsEnum = {
//    
//    val setChooser = Depend.memoized(
//      (self: Depend[(Int, Set[Int]), Set[Int]], pair: (Int, Set[Int])) => {
//        val (size, set) = pair
//
//        if (size <= 0) e.Empty: e.Enum[Set[Int]]
//        else if (size == 1) e.Enum((set map { Set(_) }).toList): e.Enum[Set[Int]]
//        else if (size <= set.size) {
//          val roots = Enum(set.toList)
//
//          val keptIn: Depend[Int, Set[Int]] = new InMap(self, { (chosen: Int) =>
//            (size - 1, set - chosen)
//          })
//
//          val leftOutIn: Depend[Int, Set[Int]] = new InMap(self, { (leftOut: Int) =>
//            (size, set - leftOut)
//          })
//          
//          val kept = memoization.Chain[Int, Set[Int], Set[Int]](roots, keptIn,
//            (root: Int, setOut: Set[Int]) => {
//              setOut + root
//            }
//          )
//          
//          val leftOut = memoization.Chain(roots, leftOutIn,
//            (root: Int, setOut: Set[Int]) => {
//              setOut
//            }
//          )
//          
//          val allNodes = e.Product(kept, leftOut)
//          allNodes: e.Enum[Set[Int]]
//        } else e.Empty: e.Enum[Set[Int]]
//      })
//      
//    setChooser
//  }

}
