package insynth.enumeration
package member
package testcase

import insynth.{ enumeration => e }
import dependent._
import memoization._

import util._
import insynth.util.logging._
import insynth.util._

import insynth.enumeration.common.RedBlackTreeTest

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.Matchers._
import org.scalacheck.Gen

import scala.language.postfixOps

// you want this to run within Eclipse
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RedBlackTreeTestingTest extends FunSuite with Matchers
  with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {
  import Checks._
  import structures._
  import RedBlackTrees._
  import RedBlackTreeTest._

//  test("correct enumeration") {
//
//    testCorrectness(constructEnumerator)
//
//  }
//
//  test("correct enumeration of other type of redblack trees") {
//
//    // construct HO enumerator of two types of red-black trees
//    val enumeratorType1 = constructEnumerator
//    val enumeratorType2 = constructEnumeratorOtherType
//
//    // compare enumerators for different parameters
//    forAll(Gen.choose(1, 5), Gen.choose(1, 5), minSuccessful(20)) {
//      (size: Int, blackHeight: Int) =>
//        {
//          whenever(blackHeight < size) {
//            val enum1 = enumeratorType1(size, 1 to size, Set(true, false), blackHeight)
//            val enum2 = enumeratorType2(size, 1 to size, Set(true, false), blackHeight)
//
//            enum1.size shouldBe enum2.size
//
//            for (ind <- 0 until enum1.size) {
//              enum1(ind) should be(enum2(ind): structures.RedBlackTrees.Tree)
//            }
//
//            for (ind <- 0 until enum1.size) {
//              enum1.member(enum2(ind)) should be(true)
//            }
//          }
//        }
//    }
//
//  }
//
//  test("correct insert operation") {
//
//    val dependEnum = constructEnumeratorOtherType
//
//    forAll(Gen.choose(1, 5), Gen.choose(1, 5), minSuccessful(20)) {
//      (size: Int, blackHeight: Int) =>
//        {
//          whenever(blackHeight < size) {
//            val enum = dependEnum(size - 1, 1 to size, Set(true, false), blackHeight)
//
//            for (ind <- 0 until enum.size) {
//              val missing = (1 to size).toList.find(!enum(ind).contains(_))
//              assert(!missing.isEmpty)
//
//              val newTree = enum(ind) insert missing.get
//
//              withClue("Old tree: %s; New tree: %s".format(enum(ind), newTree)) {
//                RedBlackTrees.size(newTree) shouldBe size
//
//                assertIndividualInvariants(newTree)
//
//                invariant(newTree) shouldBe true
//              }
//            }
//
//          }
//        }
//    }
//
//  }
//
//  test("correct member after insert operation") {
//
//    val dependEnum = constructEnumeratorOtherType
//
//    forAll(Gen.choose(1, 5), Gen.choose(1, 5), minSuccessful(20)) {
//      (size: Int, blackHeight: Int) =>
//        {
//          whenever(blackHeight < size) {
//            val enum = dependEnum(size - 1, 1 to size, Set(true, false), blackHeight)
//
//            for (ind <- 0 until enum.size) {
//              val missing = (1 to size).toList.find(!enum(ind).contains(_))
//              assert(!missing.isEmpty)
//
//              val newTree = enum(ind) insert missing.get
//
//              withClue("Old tree: %s; New tree: %s".format(enum(ind), newTree)) {
//                (0 to size).toList.exists { bH: Int =>
//                  val enumBigger = dependEnum(size, 1 to size, Set(true, false), bH)
//
//                  enumBigger.member(newTree)
//                }
//              }
//            }
//
//          }
//        }
//    }
//
//  }

  test("Comparison of times for testing after insertion") {
    
    val normalStopWatch = new Stopwatch("checking invariant cummulative time") 
    val memberStopWatch = new Stopwatch("checking membership cummulative time")

    forAll(Gen.choose(4, 6), Gen.choose(1, 5), minSuccessful(30)) {
      (size: Int, blackHeight: Int) =>
        {
          whenever(blackHeight < size) {

            val dependEnumNormal = constructEnumeratorOtherTypeMemoized

            profile("Normal:") {
              normalStopWatch.profile {
                val enum = dependEnumNormal(size - 1, 1 to size, Set(true, false), blackHeight)
  
                for (ind <- 0 until enum.size) {
                  val missing = (1 to size).toList.find(!enum(ind).contains(_))
                  assert(!missing.isEmpty)
  
                  val newTree = enum(ind) insert missing.get
  
                  withClue("Old tree: %s; New tree: %s".format(enum(ind), newTree)) {
                    RedBlackTrees.size(newTree) shouldBe size
  
                    invariant(newTree) shouldBe true
                  }
                }
              }
            }

            val dependEnumMember = constructEnumeratorOtherTypeMemoized

            profile("Member:") {
              memberStopWatch.profile {
                val enum = dependEnumMember(size - 1, 1 to size, Set(true, false), blackHeight)
  
                for (ind <- 0 until enum.size) {
                  val missing = (1 to size).toList.find(!enum(ind).contains(_))
                  assert(!missing.isEmpty)
  
                  val newTree = enum(ind) insert missing.get
  
                  withClue("Old tree: %s; New tree: %s".format(enum(ind), newTree)) {
                    (blackHeight to (blackHeight + 1)).toList.exists { bH: Int =>
                      val enumBigger = dependEnumMember(size, 1 to size, Set(true, false), bH)
  
                      enumBigger.member(newTree)
                    } shouldBe true
                  }
                }
              }
            }

          }
        }
    }
    
    println("m: " + memberStopWatch.acc + "n: " + normalStopWatch.acc)
    memberStopWatch.acc should be < normalStopWatch.acc
  }
//
//  test("Correct memoization") {
//
//    forAll(Gen.choose(1, 5), minSuccessful(20)) {
//      (size: Int) =>
//        {
//          val dependEnum = constructEnumeratorOtherTypeMemoized
//
//          val enum = dependEnum(size, 1 to size, Set(true, false), Math.log2(size + 1).toInt)
//          enum.toList
//
//          for (innerSize <- 1 until size) {
//            val log2Size = Math.log2(innerSize + 1).toInt;
//
//            val paramsOfMemoized =
//              for (
//                bh <- log2Size - 1 to log2Size + 1; bs <- List(Set(true), Set(true, false));
//                if (dependEnum.memoizedMap.keys.toSet contains
//                  (innerSize, 1 to innerSize, Set(true, false), bh))
//              ) yield (bh, bs)
//            paramsOfMemoized.size should be >= 1
//
//            for ((bh, bs) <- paramsOfMemoized) {
//              val innerEnum = dependEnum(innerSize, 1 to innerSize, bs, bh)
//
//              for (j <- 0 until innerEnum.size)
//                withClue(innerEnum + "did not memoize index " + j) {
//                  innerEnum.asInstanceOf[e.memoization.Memoized[_]].memoizedFlags contains j shouldBe true
//                }
//            }
//
//          }
//        }
//    }
//
//  }
//
//  test("correct member after insert operation, with memoization") {
//
//    val dependEnum = constructEnumeratorOtherTypeMemoized
//
//    forAll(Gen.choose(1, 5), Gen.choose(1, 5), minSuccessful(20)) {
//      (size: Int, blackHeight: Int) =>
//        {
//          whenever(blackHeight < size) {
//            val enum = dependEnum(size - 1, 1 to size, Set(true, false), blackHeight)
//
//            for (ind <- 0 until enum.size) {
//              val missing = (1 to size).toList.find(!enum(ind).contains(_))
//              assert(!missing.isEmpty)
//
//              val newTree = enum(ind) insert missing.get
//
//              withClue("Old tree: %s; New tree: %s".format(enum(ind), newTree)) {
//                (0 to size).toList.exists { bH: Int =>
//                  val enumBigger = dependEnum(size, 1 to size, Set(true, false), bH)
//
//                  //            withClue("Enum: %s, enum of bigger trees: %s".format(enum.mkString("\n"), enumBigger.mkString("\n"))) {
//                  //              enumBigger.size should be > enum.size
//                  //            }
//                  //                withClue("All trees: %s".format(enumBigger.toList.mkString("\n"))) {
//                  //                  enumBigger.toList should contain (newTree)
//                  //                }
//
//                  enumBigger.member(newTree)
//                }
//              }
//            }
//
//          }
//        }
//    }
//
//  }
//
  // constructs enumerator for "simple" red-black trees
  def constructEnumerator = {

    val colorsProducer = new WrapFunctionFin(
      (set: Set[Boolean]) => { new WrapArray(set.toArray) })

    val treesOfSize = new WrapFunctionFin(
      (self: MemberDependFinite[(Int, Range, Set[Boolean], Int), Tree], pair: (Int, Range, Set[Boolean], Int)) => {
        val (size, range, colors, blackHeight) = pair

        if (range.size >= size && range.size < 0 || blackHeight < 0) new Empty: MemberFinite[Tree]
        else if (size == 0 && blackHeight == 1 && colors.contains(true)) new Singleton(Leaf): MemberFinite[Tree]
        else if (size > 0 && blackHeight >= 1) {
          val roots = new WrapRange(range)
          val leftSizes = new WrapArray(0 until size toArray)
          val rootColors = colorsProducer(colors)

          val rootLeftSizePairs = new member.ProductFinite(leftSizes, roots)
          val rootLeftSizeColorTuples = new member.ProductFinite(rootLeftSizePairs, rootColors)

          val leftTrees = new InMapFin(self, { (par: ((Int, Int), Boolean)) =>
            val ((leftSize, median), rootColor) = par
            val childColors = if (rootColor) Set(true, false) else Set(true)
            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
            (leftSize, range.start to (median - 1), childColors, childBlackHeight)
          })

          val rightTrees = new InMapFin(self, { (par: ((Int, Int), Boolean)) =>
            val ((leftSize, median), rootColor) = par
            val childColors = if (rootColor) Set(true, false) else Set(true)
            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
            (size - leftSize - 1, (median + 1) to range.end, childColors, childBlackHeight)
          })

          val leftRightPairs =
            Product(leftTrees, rightTrees)

          val allNodes = new ChainFinite(rootLeftSizeColorTuples, leftRightPairs)

          val makeTree =
            (p: (((Int, Int), Boolean), (Tree, Tree))) => {
              val (((leftSize, currRoot), rootColor), (leftTree, rightTree)) = p

              assert(!(size >= 2 && leftSize == 0 && size - leftSize - 1 == 0))
              assert(!(size >= 2 && leftTree == Leaf && rightTree == Leaf))
              assert(!(leftSize > 0 && leftTree == Leaf), "leftSize=%d, leftTree=Leaf".format(leftSize))
              Node(leftTree, currRoot, rightTree, rootColor)
            }

          val invertTree = {
            (p: Tree) =>
              {
                val Node(leftTree, currRoot, rightTree, rootColor) = p.asInstanceOf[Node]

                (((RedBlackTrees.size(leftTree), currRoot), rootColor), (leftTree, rightTree))
              }
          }

          new Map[(((Int, Int), Boolean), (Tree, Tree)), Tree](allNodes, makeTree, invertTree) with MemberFinite[Tree]: MemberFinite[Tree]
        } else new Empty: MemberFinite[Tree]
      })

    treesOfSize
  }

  // constructs enumerator for red-black trees with operations
  def constructEnumeratorOtherType = {
    import RedBlackTreeWithOperations._

    val colorsProducer = new WrapFunctionFin(
      (set: Set[Boolean]) => { new WrapArray(set.toArray) })

    val treesOfSize = new WrapFunctionFin(
      (self: MemberDependFinite[(Int, Range, Set[Boolean], Int), Tree], pair: (Int, Range, Set[Boolean], Int)) => {
        val (size, range, colors, blackHeight) = pair

        if (range.size >= size && range.size < 0 || blackHeight < 0) new Empty: MemberFinite[Tree]
        else if (size == 0 && blackHeight == 1 && colors.contains(true)) new Singleton(Leaf): MemberFinite[Tree]
        else if (size > 0 && blackHeight >= 1) {
          val roots = new WrapRange(range)
          val leftSizes = new WrapArray(0 until size toArray)
          val rootColors = colorsProducer(colors)

          val rootLeftSizePairs = new member.ProductFinite(leftSizes, roots)
          val rootLeftSizeColorTuples = new member.ProductFinite(rootLeftSizePairs, rootColors)

          val leftTrees = new InMapFin(self, { (par: ((Int, Int), Boolean)) =>
            val ((leftSize, median), rootColor) = par
            val childColors = if (rootColor) Set(true, false) else Set(true)
            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
            (leftSize, range.start to (median - 1), childColors, childBlackHeight)
          })

          val rightTrees = new InMapFin(self, { (par: ((Int, Int), Boolean)) =>
            val ((leftSize, median), rootColor) = par
            val childColors = if (rootColor) Set(true, false) else Set(true)
            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
            (size - leftSize - 1, (median + 1) to range.end, childColors, childBlackHeight)
          })

          val leftRightPairs =
            Product(leftTrees, rightTrees)

          val allNodes = new ChainFinite(rootLeftSizeColorTuples, leftRightPairs)

          val makeTree =
            (p: (((Int, Int), Boolean), (Tree, Tree))) => {
              val (((leftSize, currRoot), rootColor), (leftTree, rightTree)) = p

              assert(!(size >= 2 && leftSize == 0 && size - leftSize - 1 == 0))
              assert(!(size >= 2 && leftTree == Leaf && rightTree == Leaf))
              assert(!(leftSize > 0 && leftTree == Leaf), "leftSize=%d, leftTree=Leaf".format(leftSize))
              Node(rootColor, leftTree, currRoot, rightTree)
            }

          val invertTree = {
            (p: Tree) =>
              {
                val Node(rootColor, leftTree, currRoot, rightTree) = p.asInstanceOf[Node]

                (((RedBlackTrees.size(leftTree), currRoot), rootColor), (leftTree, rightTree))
              }
          }

          new Map[(((Int, Int), Boolean), (Tree, Tree)), Tree](allNodes, makeTree, invertTree) with MemberFinite[Tree]: MemberFinite[Tree]
        } else new Empty: MemberFinite[Tree]
      })

    treesOfSize
  }

  def constructEnumeratorOtherTypeMemoized = {
    import RedBlackTreeWithOperations._

    val colorsProducer = new WrapFunctionFin(
      (set: Set[Boolean]) => { new WrapArray(set.toArray) })

    val treesOfSize = new WrapFunctionFin(
      (self: MemberDependFinite[(Int, Range, Set[Boolean], Int), Tree],
        pair: (Int, Range, Set[Boolean], Int)) => {
        val (size, range, colors, blackHeight) = pair

        if (range.size >= size && range.size < 0 || blackHeight < 0) new Empty: MemberFinite[Tree]
        else if (size == 0 && blackHeight == 1 && colors.contains(true)) new Singleton(Leaf): MemberFinite[Tree]
        else if (size > 0 && blackHeight >= 1) {
          val roots = new WrapRange(range)
          val leftSizes = new WrapArray(0 until size toArray)
          val rootColors = colorsProducer(colors)

          val rootLeftSizePairs = new member.ProductFinite(leftSizes, roots)
          val rootLeftSizeColorTuples = new member.ProductFinite(rootLeftSizePairs, rootColors)

          val leftTrees = new InMapFin(self, { (par: ((Int, Int), Boolean)) =>
            val ((leftSize, median), rootColor) = par
            val childColors = if (rootColor) Set(true, false) else Set(true)
            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
            (leftSize, range.start to (median - 1), childColors, childBlackHeight)
          })

          val rightTrees = new InMapFin(self, { (par: ((Int, Int), Boolean)) =>
            val ((leftSize, median), rootColor) = par
            val childColors = if (rootColor) Set(true, false) else Set(true)
            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
            (size - leftSize - 1, (median + 1) to range.end, childColors, childBlackHeight)
          })

          val leftRightPairs =
            Product(leftTrees, rightTrees)

          val allNodes = new ChainFinite(rootLeftSizeColorTuples, leftRightPairs)

          val makeTree =
            (p: (((Int, Int), Boolean), (Tree, Tree))) => {
              val (((leftSize, currRoot), rootColor), (leftTree, rightTree)) = p

              assert(!(size >= 2 && leftSize == 0 && size - leftSize - 1 == 0))
              assert(!(size >= 2 && leftTree == Leaf && rightTree == Leaf))
              assert(!(leftSize > 0 && leftTree == Leaf), "leftSize=%d, leftTree=Leaf".format(leftSize))
              Node(rootColor, leftTree, currRoot, rightTree)
            }

          val invertTree = {
            (p: Tree) =>
              {
                val Node(rootColor, leftTree, currRoot, rightTree) = p.asInstanceOf[Node]

                (((RedBlackTrees.size(leftTree), currRoot), rootColor), (leftTree, rightTree))
              }
          }

          new Map[(((Int, Int), Boolean), (Tree, Tree)), Tree](allNodes, makeTree, invertTree) with MemberFinite[Tree] with e.memoization.Memoized[Tree] with Memoized[Tree]: MemberFinite[Tree]
        } else new Empty: MemberFinite[Tree]
      }) with e.memoization.dependent.Memoized[(Int, Range, Set[Boolean], Int), Tree]

    treesOfSize
  }

  def constructEnumeratorOtherTypeMemoizedBlackHeight = {
    import RedBlackTreeWithOperations._

    val colorsProducer = new WrapFunctionFin(
      (set: Set[Boolean]) => { new WrapArray(set.toArray) })

    val treesOfSize = new WrapFunctionFin(
      (self: MemberDependFinite[(Int, Range, Set[Boolean], Int), Tree], pair: (Int, Range, Set[Boolean], Int)) => {
        val (size, range, colors, blackHeight) = pair

        if (range.size >= size && range.size < 0 || blackHeight < 0) new Empty: MemberFinite[Tree]
        else if (size == 0 && blackHeight == 1 && colors.contains(true)) new Singleton(Leaf): MemberFinite[Tree]
        else if (size > 0 && blackHeight >= 1) {
          val roots = new WrapRange(range)
          val leftSizes = new WrapArray(0 until size toArray)
          val rootColors = colorsProducer(colors)

          val rootLeftSizePairs = new member.ProductFinite(leftSizes, roots)
          val rootLeftSizeColorTuples = new member.ProductFinite(rootLeftSizePairs, rootColors)

          val leftTrees = new InMapFin(self, { (par: ((Int, Int), Boolean)) =>
            val ((leftSize, median), rootColor) = par
            val childColors = if (rootColor) Set(true, false) else Set(true)
            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
            (leftSize, range.start to (median - 1), childColors, childBlackHeight)
          })

          val rightTrees = new InMapFin(self, { (par: ((Int, Int), Boolean)) =>
            val ((leftSize, median), rootColor) = par
            val childColors = if (rootColor) Set(true, false) else Set(true)
            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
            (size - leftSize - 1, (median + 1) to range.end, childColors, childBlackHeight)
          })

          val leftRightPairs =
            Product(leftTrees, rightTrees)

          val allNodes = new ChainFinite(rootLeftSizeColorTuples, leftRightPairs)

          val makeTree =
            (p: (((Int, Int), Boolean), (Tree, Tree))) => {
              val (((leftSize, currRoot), rootColor), (leftTree, rightTree)) = p

              assert(!(size >= 2 && leftSize == 0 && size - leftSize - 1 == 0))
              assert(!(size >= 2 && leftTree == Leaf && rightTree == Leaf))
              assert(!(leftSize > 0 && leftTree == Leaf), "leftSize=%d, leftTree=Leaf".format(leftSize))
              Node(rootColor, leftTree, currRoot, rightTree)
            }

          val invertTree = {
            (p: Tree) =>
              {
                val Node(rootColor, leftTree, currRoot, rightTree) = p.asInstanceOf[Node]

                (((RedBlackTrees.size(leftTree), currRoot), rootColor), (leftTree, rightTree))
              }
          }

          new Map[(((Int, Int), Boolean), (Tree, Tree)), Tree](allNodes, makeTree, invertTree) with MemberFinite[Tree] with e.memoization.Memoized[Tree] with Memoized[Tree]: MemberFinite[Tree]
        } else new Empty: MemberFinite[Tree]
      }) with e.memoization.dependent.Memoized[(Int, Range, Set[Boolean], Int), Tree]

    treesOfSize
  }

  def constructEnumeratorNormal = {
    import e.dependent._

    val colorsProducer = Depend(
      (set: Set[Boolean]) => { e.WrapArray(set.toArray) })

    val treesOfSize: Depend[(Int, Range, Set[Boolean], Int), Tree] = Depend(
      (self: Depend[(Int, Range, Set[Boolean], Int), Tree], pair: (Int, Range, Set[Boolean], Int)) => {
        val (size, range, colors, blackHeight) = pair

        if (range.size >= size && range.size < 0 || blackHeight < 0) e.Empty
        else if (size == 0 && blackHeight == 1 && colors.contains(true)) e.Singleton(Leaf)
        //        else if (size == 1 && blackHeight == 1 && colors.contains(false)) e.WrapArray(range map { v => Node(Leaf, v, Leaf, false) })
        //        else if (size == 1 && blackHeight == 2 && colors.contains(true)) e.WrapArray(range map { v => Node(Leaf, v, Leaf, true) })
        //        else if (size == 1) e.WrapArray(range map { v => Node(Leaf, v, Leaf, true) })
        else if (size > 0 && blackHeight >= 1) {
          val roots = e.Enum(range)
          val leftSizes = e.WrapArray(0 until size)
          val rootColors = colorsProducer(colors)

          val rootLeftSizePairs = e.Product(leftSizes, roots)
          val rootLeftSizeColorTuples = e.Product(rootLeftSizePairs, rootColors)

          val leftTrees: Depend[((Int, Int), Boolean), Tree] = InMap(self, { (par: ((Int, Int), Boolean)) =>
            val ((leftSize, median), rootColor) = par
            val childColors = if (rootColor) Set(true, false) else Set(true)
            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
            (leftSize, range.start to (median - 1), childColors, childBlackHeight)
          })

          val rightTrees: Depend[((Int, Int), Boolean), Tree] = InMap(self, { (par: ((Int, Int), Boolean)) =>
            val ((leftSize, median), rootColor) = par
            val childColors = if (rootColor) Set(true, false) else Set(true)
            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
            (size - leftSize - 1, (median + 1) to range.end, childColors, childBlackHeight)
          })

          val leftRightPairs: Depend[((Int, Int), Boolean), (Tree, Tree)] =
            Product(leftTrees, rightTrees)

          val allNodes =
            Chain[((Int, Int), Boolean), (Tree, Tree), Node](rootLeftSizeColorTuples, leftRightPairs,
              (p1: ((Int, Int), Boolean), p2: (Tree, Tree)) => {
                val (((leftSize, currRoot), rootColor), (leftTree, rightTree)) = (p1, p2)

                assert(!(size >= 2 && leftSize == 0 && size - leftSize - 1 == 0))
                assert(!(size >= 2 && leftTree == Leaf && rightTree == Leaf))
                assert(!(leftSize > 0 && leftTree == Leaf), "leftSize=%d, leftTree=Leaf".format(leftSize))
                Node(leftTree, currRoot, rightTree, rootColor)
              })

          allNodes
        } else e.Empty
      })

    treesOfSize
  }

}
