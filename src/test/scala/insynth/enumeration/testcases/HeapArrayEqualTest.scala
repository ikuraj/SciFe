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
import structures.BSTrees._

import org.scalatest._
import org.scalameter.api._

import scala.language.postfixOps
import scala.language.existentials

class HeapArrayEqualTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
	HasLogger with ProfileLogger { 
  import Checks._
  import structures._
  import BSTrees._
  import Util._
  
  val bound = 6
  
  test("heap enumeration") {
    val checkerHelper = new CheckerHelper[Tree]
    import checkerHelper._
    
    def rangeList(m: Int) = m to 0 by -1 toArray
    val enum = heapsEnum

    withLazyClue("Elements are: " + clue) {
	    for (m <- 1 to bound) {
      	res = enum.getEnum( (0, rangeList(m: Int)) )
      	res.size should be (1)
      	elements should contain only (Leaf)
	    }

	    for (m <- 2 to bound) {
      	res = enum.getEnum( (1, rangeList(m: Int)) )
      	res shouldBe a [WrapArray[_]]
      	res.size should be (rangeList(m).size)
	    }

//	    forAll ("size", "range max") { (s: Int, m: Int) =>
//	      whenever (m > 0 && s < 10 && m < s) {
	    	for(s <- 1 to bound; m <- 1 to s) {
	        addMessage = "m=%d and s=%d".format(m, s)
	        res = enum.getEnum( (m, rangeList(m: Int)) )

//	        res.size should be (listCombinations.size)
	        elements.forall( heapEqualProperty(_) ) should be (true)
    		}
//	    }
	    	
      addMessage = ""

    	res = enum.getEnum( (3, rangeList(3)) )
      elements.size should be (elements.distinct.size)
//    	res = enum.getEnum( (3, rangeList(3)) )
//      elements.size should be (elements.distinct.size)
//    	res.size should be (30)
//    	res = enum.getEnum( (4, rangeList(4)) )
//      elements.size should be (elements.distinct.size)
//    	res.size should be (30)
//    	res = enum.getEnum( (7, rangeList(7)) )
//      elements.size should be (elements.distinct.size)
//    	res.size should be (73644)
    }
  }

//  test("heap enumeration 2") {
//    val checkerHelper = new CheckerHelper[Tree]
//    import checkerHelper._
//    
//    def rangeList(m: Int) = m to 1 by -1 toArray
//    val enum = heapsEnum2
//
//    withLazyClue("Elements are: " + clue) {
//	    for (m <- 1 to 10) {
//      	res = enum.getEnum( (0, rangeList(m: Int)) )
//      	res.size should be (1)
//      	elements should contain only (Leaf)
//	    }
//
//	    for (m <- 2 to 10) {
//      	res = enum.getEnum( (1, rangeList(m: Int)) )
//      	res shouldBe a [WrapArray[_]]
//      	res.size should be (rangeList(m).size)
//	    }
//
////	    forAll ("size", "range max") { (s: Int, m: Int) =>
////	      whenever (m > 0 && s < 10 && m < s) {
//	    	for(s <- 1 to 10; m <- 1 to s) {
//	        addMessage = "m=%d and s=%d".format(m, s)
//	        res = enum.getEnum( (m, rangeList(m: Int)) )
//
////	        res.size should be (listCombinations.size)
//	        elements.forall( heapEqualProperty(_) ) should be (true)
//    		}
////	    }
//	    	
//      addMessage = ""
//
//    	res = enum.getEnum( (3, rangeList(3)) )
//      elements.size should be (elements.distinct.size)
//    	res.size should be (14)
//    	res = enum.getEnum( (7, rangeList(7)) )
//      elements.size should be (elements.distinct.size)
//    	res.size should be (73644)
//    }
//  }

  
	def getRange(m: Int) = m to 0 by -1
  test("heap enumeration 3") {
    val checkerHelper = new CheckerHelper[Tree]
    import checkerHelper._
    
    def rangeList(m: Int) = m to 1 by -1 toArray
    implicit val memScope = new MemoizationScope
    val enum = heapEnum3(memScope)

    withLazyClue("Elements are: " + clue) {
	    for (m <- 1 to bound) {
      	res = enum.getEnum( (0, getRange(m: Int)) )
      	res.size should be (1)
      	elements should contain only (Leaf)
	    }

	    for (m <- 2 to bound) {
      	res = enum.getEnum( (1, getRange(m: Int)) )
      	res shouldBe a [WrapArray[_]]
      	res.size should be (getRange(m).size)
	    }

	    	for(s <- 1 to bound; m <- 1 to s) {
	        addMessage = "m=%d and s=%d".format(m, s)
	        res = enum.getEnum( (m, getRange(m: Int)) )

	        elements.forall( heapEqualProperty(_) ) should be (true)
	        elements.distinct.size should be (elements.size)
    		}
      addMessage = ""

//      for ((exp, maxSize) <- List(4, 15, 66, 320).zipWithIndex ) {
//	      val all =
//		      for (i <- (1 to maxSize +1) ) yield {
//		        res = enum.getEnum( (i, getRange(i)) )
//        		addMessage += res.toList.mkString("\n")
//		        res.size
//		      }
//	      all.sum should be (exp)
//      }
//      {
//	      val all =
//		      for (i <- 1 to 4) yield {
//		        res = enum.getEnum( (i, getRange(i)) )
//		        res.size
//		      }
//	      all.sum should be (320)
//      }
//      {
//	      val all =
//		      for (i <- 1 to 6) yield {
//		        res = enum.getEnum( (i, getRange(i)) )
//		        res.size
//		      }
//	      all.sum should be (13139)
//      }
        
    	res = enum.getEnum( (3, getRange(3)) )
      elements.size should be (elements.distinct.size)
    	res.size should be (30)
//    	res = enum.getEnum( (7, getRange(7)) )
//      elements.size should be (elements.distinct.size)
//    	res.size should be (73618)
    }
  }

  test("heap enumeration 4") {
    val checkerHelper = new CheckerHelper[Tree]
    import checkerHelper._
    
    def rangeList(m: Int) = m to 1 by -1 toArray
    implicit val memScope = new MemoizationScope
    val enum = heapEnum4(memScope)

    withLazyClue("Elements are: " + clue) {
	    for (m <- 1 to bound) {
      	res = enum.getEnum( (0, getRange(m: Int)) )
      	res.size should be (1)
      	elements should contain only (Leaf)
	    }

	    for (m <- 2 to bound) {
      	res = enum.getEnum( (1, getRange(m: Int)) )
      	res shouldBe a [WrapArray[_]]
      	res.size should be (getRange(m).size)
	    }

	    	for(s <- 1 to bound; m <- 1 to s) {
	        addMessage = "m=%d and s=%d".format(m, s)
	        res = enum.getEnum( (m, getRange(m: Int)) )

	        elements.forall( heapEqualProperty(_) ) should be (true)
	        elements.distinct.size should be (elements.size)
    		}
      addMessage = ""

    	res = enum.getEnum( (3, getRange(3)) )
      elements.size should be (elements.distinct.size)
    	res.size should be (30)
//    	res = enum.getEnum( (7, getRange(7)) )
//      elements.size should be (elements.distinct.size)
//    	res.size should be (73618)
//    	res = enum.getEnum( (8, getRange(8)) )
//      elements.size should be (elements.distinct.size)
//    	res.size should be < 0
//    	for (i <- 0 until Int.MaxValue)	res(i)
    }
  }

  def heapEnum4(implicit ms: MemoizationScope) = {
  	type EnumType = Depend[(Int, Range), Tree]

    Depend.memoized(
      (self: EnumType, pair: (Int, Range)) => {
      // list sorted descendingly
      val (size, range) = pair

      if (size > range.size) e.Empty
      else 
      if (size <= 0) e.Singleton(Leaf)
      else if (size == 1) {
//        (e.Enum(list): Enum[Int]) map { v => Node(Leaf, v, Leaf) }
//        e.WrapArray( array map { v => Node(Leaf, v, Leaf) } )
        val arr = new Array[Node](range.size)
        for (i <- 0 until range.size) arr(i) = Node(Leaf, range.start - i, Leaf)
        e.WrapArray(arr)
      }
      else if (!range.isEmpty) {
        val rootsInds = Enum(range): Finite[Int]

        val leftSize = size/2
        val childHeaps = InMap(self, { (rootInd: Int) =>
          ( leftSize, getRange(rootInd): Range )
        })
        val leftRightPairs: Depend[Int, (Tree, Tree)] =
          Product(childHeaps, childHeaps)
        
        val allNodes =
          memoization.Chain[Int, (Tree, Tree), Node](rootsInds, leftRightPairs,
            (rootInd: Int, p2: (Tree, Tree)) => {
              val (leftTree, rightTree) = p2

              Node(leftTree, rootInd, rightTree)
            })

        allNodes
      } else e.Empty
    })
  }

  def heapEnum3(implicit ms: MemoizationScope) = {
  	type EnumType = Depend[(Int, Range), Tree]

    Depend.memoized(
      (self: EnumType, pair: (Int, Range)) => {
      // list sorted descendingly
      val (size, range) = pair
      info("(size, array.size)" + (size, range.size))

      if (size > range.size) e.Empty
      else if (size <= 0) e.Singleton(Leaf)
      else if (size == 1) {
//        (e.Enum(list): Enum[Int]) map { v => Node(Leaf, v, Leaf) }
//        e.WrapArray( array map { v => Node(Leaf, v, Leaf) } )
        val arr = new Array[Node](range.size)
        for (i <- 0 until range.size) arr(i) = Node(Leaf, range.start - i, Leaf)
        e.WrapArray(arr)
      }
      else if (!range.isEmpty) {
        val rootsInds = Enum(range): Finite[Int]

        val leftSize = (size-1)/2
        val childHeaps1 = InMap(self, { (rootInd: Int) =>
          ( size - 1 - leftSize, getRange(rootInd): Range )
        })
        val childHeaps2 = InMap(self, { (rootInd: Int) =>
          ( leftSize, getRange(rootInd): Range )
        })
        val leftRightPairs: Depend[Int, (Tree, Tree)] =
          Product(childHeaps1, childHeaps2)
        
        val allNodes =
          memoization.Chain[Int, (Tree, Tree), Node](rootsInds, leftRightPairs,
            (rootInd: Int, p2: (Tree, Tree)) => {
              val (leftTree, rightTree) = p2

              Node(leftTree, rootInd, rightTree)
            })

        allNodes
      } else e.Empty
    })
  }

  def heapsEnum2 = {
  		Depend.memoized(
      (self: Depend[(Int, Array[Int]), Tree], pair: (Int, Array[Int])) => {
        // list sorted descendingly
        val (size, list) = pair

        if (size <= 0) e.Singleton(Leaf): Finite[Tree]
        else if (size == 1)
          e.WrapArray( (list map { v => Node(Leaf, v, Leaf) }) ): Finite[Tree]
//          (e.Enum(list): Enum[Int]) map { v => Node(Leaf, v, Leaf) }
        else if (!list.isEmpty) {
          val rootsInds = Enum(0 until list.size)

          val leftSize = size/2
          val childHeaps1 = InMap(self, { (rootInd: Int) =>
            ( leftSize, list.drop(rootInd): Array[Int] )            
          })
          val childHeaps2 = InMap(self, { (rootInd: Int) =>
            ( size - leftSize, list.drop(rootInd): Array[Int] )            
          })
          val leftRightPairs: Depend[Int, (Tree, Tree)] =
            Product(childHeaps1, childHeaps2)
          
          val allNodes =
          		memoization.Chain[Int, (Tree, Tree), Node](rootsInds, leftRightPairs,
              (rootInd: Int, p2: (Tree, Tree)) => {
                val (leftTree, rightTree) = p2

                Node(leftTree, list(rootInd), rightTree)
              })

          allNodes
        } else e.Empty
      })
  }

  def heapsEnum = {
  		Depend.memoized(
      (self: Depend[(Int, Array[Int]), Tree], pair: (Int, Array[Int])) => {
        // list sorted descendingly
        val (size, list) = pair

        if (size <= 0) e.Singleton(Leaf): Finite[Tree]
        else if (size == 1)
          e.WrapArray( (list map { v => Node(Leaf, v, Leaf) }) ): Finite[Tree]
//          (e.Enum(list): Enum[Int]) map { v => Node(Leaf, v, Leaf) }
        else if (!list.isEmpty) {
          val rootsInds = Enum(0 until list.size)

          val childHeaps = InMap(self, { (rootInd: Int) =>
            ( (size-1)/2, list.drop(rootInd): Array[Int] )            
          })
          val leftRightPairs: Depend[Int, (Tree, Tree)] =
            Product(childHeaps, childHeaps)
          
          val allNodes =
          		memoization.Chain[Int, (Tree, Tree), Node](rootsInds, leftRightPairs,
              (rootInd: Int, p2: (Tree, Tree)) => {
                val (leftTree, rightTree) = p2

                Node(leftTree, list(rootInd), rightTree)
              })

          allNodes
        } else e.Empty
      })
  }
  
}
