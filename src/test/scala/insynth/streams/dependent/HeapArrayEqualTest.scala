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

class HeapArrayEqualTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
	HasLogger with ProfileLogger {  
  import e._
  
  import Checks._
  import Structures._
  import BSTrees._
  
  test("heap enumeration") {
    val checkerHelper = new CheckerHelper[Tree]
    import checkerHelper._
    
    def rangeList(m: Int) = m to 0 by -1 toList
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
      	res.size should be (rangeList(m).size)
	    }

//	    forAll ("size", "range max") { (s: Int, m: Int) =>
//	      whenever (m > 0 && s < 10 && m < s) {
	    	for(s <- 1 to 10; m <- 1 to s) {
	        addMessage = "m=%d and s=%d".format(m, s)
	        res = enum.getStream( (m, rangeList(m: Int)) )

//	        res.size should be (listCombinations.size)
	        elements.forall( heapEqualProperty(_) ) should be (true)
    		}
//	    }
	    	
      addMessage = ""

    	res = enum.getStream( (3, rangeList(3)) )
      elements.size should be (elements.distinct.size)
    	res.size should be (30)
    	res = enum.getStream( (7, rangeList(7)) )
      elements.size should be (elements.distinct.size)
    	res.size should be (73644)
    }
  }

  def heapsEnum = {
    Producer.memoized(
      (self: Dependent[(Int, List[Int]), Tree], pair: (Int, List[Int])) => {
        // list sorted descendingly
        val (size, list) = pair

        if (size <= 0) e.Singleton(Leaf)
        else if (size == 1)
          (e.Enum(list): Enum[Int]) map { v => Node(Leaf, v, Leaf) }
        else if (!list.isEmpty) {
          val rootsInds = Enum(0 until list.size)

          val childHeaps = new InMapper(self, { (rootInd: Int) =>
            ( (size-1)/2, list.drop(rootInd) )
          })
          val leftRightPairs: Dependent[Int, (Tree, Tree)] =
            CoupledBinary(childHeaps, childHeaps)
          
          import BinaryFiniteMemoized._

          val allNodes =
            combine[Int, (Tree, Tree), Node](rootsInds, leftRightPairs,
              (rootInd: Int, p2: (Tree, Tree)) => {
                val (leftTree, rightTree) = p2

                Node(leftTree, list(rootInd), rightTree)
              })

          allNodes
        } else e.Empty
      })
  }
  
}
