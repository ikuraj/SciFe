package insynth
package streams
package dependent

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._

import streams.{ light => e }

import util._
import util.format._
import util.logging._
import common._

import scala.language.postfixOps

class HeapArrayTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
	HasLogger with ProfileLogger {  
  import e._
  
  import Checks._
  import Structures._
  import BSTrees._
  
  test("sublists enumeration") {
    val checkerHelper = new CheckerHelper[List[Int]]
    import checkerHelper._

    val setChooser = sublistsEnum

    withLazyClue("Elements are: " + clue) {
	    for (m <- 1 to 10) {
      	res = setChooser.getStream( (0, 1 to m toList) )
      	res.size should be (0)
	    }

	    for (m <- 2 to 10) {
      	res = setChooser.getStream( (1, 1 to m toList) )
      	res shouldBe a [WrapperArray[_]]
      	res.size should be (m)
      	elements should contain theSameElementsAs( 1 to m map { List(_) } )
	    }

//	    forAll ("size", "range max") { (s: Int, m: Int) =>
//	      whenever (m > 0 && s < 10 && m < s) {
	    	for(s <- 1 to 10; m <- 1 to s) {
	        addMessage = "m=%d and s=%d".format(m, s)
	        res = setChooser.getStream( (m, 1 to s toList) )
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
      	res = enum.getStream( (0, rangeList(m: Int)) )
      	res.size should be (1)
      	elements should contain only (Leaf)
	    }

	    for (m <- 2 to 10) {
      	res = enum.getStream( (1, rangeList(m: Int)) )
      	res shouldBe a [Mapper[_, _]]
      	res.size should be (m)
	    }

//	    forAll ("size", "range max") { (s: Int, m: Int) =>
//	      whenever (m > 0 && s < 10 && m < s) {
	    	for(s <- 1 to 10; m <- 1 to s) {
	        addMessage = "m=%d and s=%d".format(m, s)
	        res = enum.getStream( (m, rangeList(m: Int)) )

//	        res.size should be (listCombinations.size)
	        elements.forall( heapProperty(_) ) should be (true)
    		}
//	    }
	    	
      addMessage = ""

    	res = enum.getStream( (7, rangeList(7)) )
    	res.size should be (13139)
    }
  }

  def sublistsEnum = {
    
    val listChooser = Producer.memoized(
      (self: Dependent[(Int, List[Int]), List[Int]], pair: (Int, List[Int])) => {
        val (size, list) = pair

        if (size <= 0) e.Empty: e.Enum[List[Int]]
        else if (size == 1) e.Enum((list map { List(_) }).toList): e.Enum[List[Int]]
        else if (size <= list.size) {
          val kept = self.getStream( (size - 1, list.tail) ) map { list.head :: _ }
          val leftOut = self.getStream( (size, list.tail) )
          
          val allNodes = e.RoundRobbin(kept, leftOut)
          allNodes
        } else e.Empty
      })
      
    listChooser
  }
  
  def heapsEnum = {
    Producer.memoized(
      (self: Dependent[(Int, List[Int]), Tree], pair: (Int, List[Int])) => {
        // list sorted descendingly
        val (size, list) = pair

        if (size <= 0) e.Singleton(Leaf)
        else if (size == 1)
          (e.Enum(list): Enum[Int]) map { v => Node(Leaf, v, Leaf) }
        else if (size <= list.size) {
          val root = list.head
          val sublists = sublistsEnum.getStream( ((size-1)/2, list.tail) )

          val leftHeaps = new InMapper(self, { (left: List[Int]) =>
            ( (size-1)/2, left )
          })
          val rightHeaps = new InMapper(self, { (left: List[Int]) =>
            ( (size-1)/2, list.tail.diff(left) )
          })
          val leftRightPairs: Dependent[List[Int], (Tree, Tree)] =
            CoupledBinary(leftHeaps, rightHeaps)
          
          import BinaryFiniteMemoized._

          val allNodes =
            combine[List[Int], (Tree, Tree), Node](sublists, leftRightPairs,
              (leftElements: List[Int], p2: (Tree, Tree)) => {
                val (leftTree, rightTree) = p2

                Node(leftTree, root, rightTree)
              })

          allNodes
        } else e.Empty
      })
  }
  
  def subsetsEnum = {
    
    val setChooser = Producer.memoized(
      (self: Dependent[(Int, Set[Int]), Set[Int]], pair: (Int, Set[Int])) => {
        val (size, set) = pair

        if (size <= 0) e.Empty: e.Enum[Set[Int]]
        else if (size == 1) e.Enum((set map { Set(_) }).toList): e.Enum[Set[Int]]
        else if (size <= set.size) {
          val roots = Enum(set)

          val keptIn: Dependent[Int, Set[Int]] = new InMapper(self, { (chosen: Int) =>
            (size - 1, set - chosen)
          })

          val leftOutIn: Dependent[Int, Set[Int]] = new InMapper(self, { (leftOut: Int) =>
            (size, set - leftOut)
          })
          
          val kept = BinaryFiniteMemoized.combine(roots, keptIn,
            (root: Int, setOut: Set[Int]) => {
              setOut + root
            }
          )
          
          val leftOut = BinaryFiniteMemoized.combine(roots, leftOutIn,
            (root: Int, setOut: Set[Int]) => {
              setOut
            }
          )
          
          val allNodes = e.RoundRobbin(kept, leftOut)
          allNodes
        } else e.Empty
      })
      
    setChooser
  }

}
