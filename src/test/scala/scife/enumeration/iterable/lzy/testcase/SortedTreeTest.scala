package scife
package enumeration
package iterable
package lzy
package testcase

import scife.{ enumeration => e }
import e.iterable._
import e.dependent._

import scalaz.LazyTuple2

import scife.util.logging._
import scife.util._

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

import scala.language.postfixOps

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class SortedTreeTest extends FunSuite with Matchers
  with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {

  import common._
  import enumdef._
  import scife.util.structures._
  import LazyBSTrees._

  type EnumType[A] = LazyEnumFinite[A] /* with Memoized[A]*/
  type DepEnumType[I, O] = Depend[I, O] { type EnumSort[A] = SortedTreeTest.this.EnumType[A] }
  type DepEnumTypeFinite[I, O] = DependFinite[I, O] { type EnumSort[A] = SortedTreeTest.this.EnumType[A] }

  implicit val ms = e.memoization.scope.NoScope

  import e.common.enumdef.BinarySearchTreeEnum.{ constructEnumeratorBenchmark => constructStrict }

  implicit val lazy2reg = LazyBSTrees.toRegularBSTTree _

  //  ignore("correctness of enumeration") {
  //    val enum = constructEnumerator
  //    val base = constructStrict
  //
  //    for (size <- 0 to 5) {
  //      val range = 1 to size
  //      enum( (size, range) ).size shouldBe base(size, range).size
  //      for(i <- 0 until enum.size)
  //        (enum( (size, range) ).next: BSTrees.Tree) shouldBe base(size, range)(i)
  //    }
  //
  //  }
  ////  
  ////  test("finding an element in a list") {
  ////    val listEnum = constructEnumerator
  ////    val testTuples =
  ////      new iterable.ProductFinite[Int, Collection[Int]](
  ////        new e.WrapArray( Array(3, 3, 3) ) with ResetIter[Int],
  ////        listEnum(3, 3)
  ////      )
  ////      
  ////    testTuples.size shouldBe 30
  ////      
  ////    for (i <- 1 to 3) {
  ////      testTuples.hasNext shouldBe true
  ////      val p = testTuples.next
  ////      p._2.mkString("")
  ////    }
  ////    testTuples.reset
  ////
  ////    for (i <- 1 to testTuples.size) {
  ////      testTuples.hasNext shouldBe true
  ////      val p = testTuples.next
  ////      p._2.mkString("")
  ////    }
  ////    testTuples.hasNext shouldBe false
  ////    testTuples.reset
  ////    
  ////    var count = 0
  ////    while (testTuples.hasNext) {
  ////      val (n, l) = testTuples.next
  ////      count += 1
  ////      val index = l.indexOf(n)
  ////    }
  ////    count shouldBe testTuples.size + (- 6 + 1) * 3 
  ////
  ////  }
  ////  
  //  test("correctness of iterative enumeration") {
  //
  ////    val size = 3
  ////    val range = 1 to 3
  ////    
  ////    val roots = Enum(range)
  ////    roots.hasNext shouldBe true
  ////    val leftSizes = Enum(0 until size)
  ////    leftSizes.hasNext shouldBe true
  ////
  ////    val rootLeftSizePairs = //new iterable.ProductFinite(leftSizes, roots) with Touchable[(Int, Int)]
  ////      iterable.lzy.Product.touchableStrictPair(leftSizes, roots)
  ////    rootLeftSizePairs.hasNext shouldBe true
  ////    rootLeftSizePairs.size shouldBe 9
  ////    for (i <- 0 until 9) rootLeftSizePairs(i)
  ////      
  ////    val whenSize1 = new e.WrapArray(Array(range map { v => Node(Leaf, v, Leaf) }: _*)) with ResetIter[Tree] with Touchable[Tree]
  ////    
  ////    val whenSize0 = new e.Singleton(Leaf) with ResetIter[Tree] with Touchable[Tree]: EnumType[Tree]
  ////    
  ////    val listAll = List(whenSize0, roots, leftSizes, whenSize1, rootLeftSizePairs )
  ////    for ((enum, ind) <- listAll.zipWithIndex; (enum2, ind2) <- listAll.zipWithIndex) {
  ////      try {
  //////        iterable.lzy.Product.touchable(enum, enum2)
  ////        iterable.lzy.Product.touchableStrictPair(enum, enum2)
  ////      } catch {
  ////        case _: java.lang.IllegalArgumentException =>
  ////          println("e1=" + ind + " , e2=" + ind2)
  ////      }
  ////    }
  //    val enum = constructEnumerator
  //    
  //    {
  //
  //      val itEnum1 = enum(1, 1 to 1)
  //      val itEnum2 = enum(2, 1 to 2).asInstanceOf[
  //        iterable.lzy.dependent.ChainFiniteSingleCombine[(Int, Int), LazyTuple2[Tree, Tree], Node]]
  //      val itEnum = enum(3, 1 to 3).asInstanceOf[
  //        iterable.lzy.dependent.ChainFiniteSingleCombine[(Int, Int), LazyTuple2[Tree, Tree], Node]]
  //      // after three times you should have hasNext == false
  //      itEnum.rightStreams.size shouldBe 3
  //      itEnum.size shouldBe 5
  //      
  //      itEnum.categoryIndex shouldBe 0
  //      itEnum.hasNext shouldBe true
  //      itEnum.categoryIndex shouldBe 0
  //      
  //      itEnum.next
  //      itEnum.categoryIndex shouldBe 0
  //      itEnum.rightStreams(0).touched shouldBe false
  //
  //      itEnum.next
  //      itEnum.next
  //      itEnum.hasNext shouldBe false
  //    }
  //
  //    for (size <- 2 to 6) {
  ////    { val size = 2; val range = 4;
  //      withClue(s"size=$size") {
  //        val itEnum = enum(size, 1 to size).asInstanceOf[
  //          iterable.lzy.dependent.ChainFiniteSingleCombine[(Int, Int), LazyTuple2[Tree, Tree], Node]]
  //        for(i <- 1 to itEnum.rightStreams.size) {
  //          itEnum.hasNext shouldBe true
  //          itEnum.next
  //        }
  //        
  //        itEnum.hasNext shouldBe false
  //      }
  //    }
  //
  //  }

  test("correctness of exhaustive iterative enumeration") {
    val denum = constructEnumerator
    val dbase = constructStrict

    for (size <- 1 to 6) {
      //    val size = 2
      val range = 1 to size

      val enum = denum((size, range))
      val base = dbase(size, range)

      enum.size shouldBe base.size
      enum.size shouldBe base.size
      enum.size shouldBe base.size
      val all =
        for (i <- 0 until base.size) yield {
          info("-------------will evaluate")
          (enum.next: BSTrees.Tree)
        }
      all should contain theSameElementsAs (base.toList)
      enum.hasNext shouldBe false
    }

    //    for (size <- 0 to 7; range <- 0 to 7; if range > size) {
    //      val itEnum = enum(size, range)
    //      val baseEnum = base(size, range)
    //
    //      itEnum.size shouldBe baseEnum.size
    //
    //      for(i <- 0 until baseEnum.size)
    //        itEnum.next shouldBe baseEnum(i)
    //      
    //      itEnum.hasNext shouldBe false
    //    }

  }
  //  
  //  test("correctness of exhaustive iterative enumeration, next") {
  //    val enum = constructEnumerator
  //
  //    for (size <- 0 to 7; range <- 0 to 7; if range > size) {
  //      val itEnum = enum(size, range)
  //      while (itEnum.hasNext) itEnum.next
  //    }
  //
  //  }
  //  
  //  test("correctness of exhaustive iterative enumeration, next, reset") {
  //    val enum = constructEnumerator
  //    val base = SortedListEnum.constructEnumerator
  //
  //    for (size <- 0 to 7; range <- 0 to 7; if range > size) {
  //      val itEnum = enum(size, range)
  //      val baseEnum = base(size, range)
  //
  //      for (_ <- 1 to 3) {
  //        itEnum.size shouldBe baseEnum.size
  //  
  //        for(i <- 0 until baseEnum.size) {
  //          itEnum.hasNext shouldBe true
  //          itEnum.next shouldBe baseEnum(i)
  //        }
  //        
  //        itEnum.hasNext shouldBe false
  //        itEnum.reset
  //      }
  //    }
  //
  //  }
  //
  //  ignore("resetting enumerator in the middle") {
  //    val depEnum = constructEnumerator
  //    val enum = depEnum(7, 7)
  //
  //    val halfOfTheEnum =
  //      for (ind <- 0 until enum.size/2)
  //        yield enum(ind)
  //        
  //    enum.reset
  //    
  //    for ((el, ind) <- halfOfTheEnum.zipWithIndex)
  //      enum(ind) should be (el)
  //
  //  } 

  // NOTE: this enumerator does not enumerate in the same order as strict
  def constructEnumerator(implicit ms: e.memoization.MemoizationScope) = {
    new WrapFunctionTest[(Int, Range), Tree, EnumType](
      (self: DepEnumType[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        //        if (range.isEmpty) iterable.lzy.Empty
        //        else
        if (size <= 0) new e.Singleton(Leaf) with ResetIter[Tree] with Touchable[Tree] {
          override def next = {
            info(s"next on Singleton${this.hashCode()} invoked!")
            super.next
          } //: EnumType[Tree]
        }
        else if (range.isEmpty) iterable.lzy.Empty
        else if (size == 1)
          new e.WrapArray(Array(range map { v => Node(Leaf, v, Leaf) }: _*)) with ResetIter[Tree] with Touchable[Tree]
        else {
          val roots = Enum(range)
          val leftSizes = Enum(0 until size)

          val rootLeftSizePairs = //new iterable.ProductFinite(leftSizes, roots) with Touchable[(Int, Int)]
            iterable.lzy.Product.touchableStrictPair(leftSizes, roots)
          assert(rootLeftSizePairs.size > 0)

          val leftTrees: DepEnumTypeFinite[(Int, Int), Tree] = new InMap(self, { (par: (Int, Int)) =>
            val (leftSize, median) = par
            (leftSize, range.start to (median - 1))
          }) with DependFinite[(Int, Int), Tree] {
            override type EnumSort[A] = SortedTreeTest.this.EnumType[A]
          }

          val rightTrees: DepEnumTypeFinite[(Int, Int), Tree] =
            new InMap(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (size - leftSize - 1, (median + 1) to range.end)
            }) with DependFinite[(Int, Int), Tree] {
              override type EnumSort[A] = SortedTreeTest.this.EnumType[A]
            }

          val leftRightPairs: iterable.lzy.dependent.LazyDependFinite[(Int, Int), LazyTuple2[Tree, Tree]] =
            iterable.lzy.dependent.ProductFinite(leftTrees, rightTrees)

          val fConstructTree: ((Int, Int), LazyTuple2[Tree, Tree]) => Tree =
            (p1, p2) => {
              info("fConstructCollection invoked!!")
              Node(p2._1, p1._2, p2._2)
            }
          //          val fConstructTree: (=> (Int, Int), => LazyTuple2[Tree, Tree]) => Tree =
          //            (p1, p2) => {
          //              info("fConstructCollection invoked!!")
          //              Node(p2._1, p1._2, p2._2)
          //            }

          val allNodes =
            new iterable.lzy.dependent.ChainFiniteSingleCombineStrict[(Int, Int), LazyTuple2[Tree, Tree], Tree](
              rootLeftSizePairs,
              leftRightPairs,
              fConstructTree) with ResetIter[Tree] with Touchable[Tree]

          if (allNodes.size > 0) allNodes
          else iterable.lzy.Empty
        }
      })
  }

}
