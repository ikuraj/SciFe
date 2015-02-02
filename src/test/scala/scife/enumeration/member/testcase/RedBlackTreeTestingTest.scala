package scife.enumeration
package member
package testcase

import scife.{ enumeration => e }

import util._
import scife.util.logging._
import scife.util._

import scife.enumeration.common.RedBlackTreeTest

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.Matchers._
import org.scalacheck.Gen

import scala.language.postfixOps

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class RedBlackTreeTestingTest extends FunSuite with Matchers
  with GeneratorDrivenPropertyChecks
  with HasLogger with ProfileLogger {
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
  //
  //
  //    // when enumerating enumerator of larger trees, enumerators of smaller trees,
  //    // coming from the same HO enumerator, should be memoized, for membership
  //    test("Testing memoization and member explicitly") {
  //  
  //      forAll(Gen.choose(3, 5), minSuccessful(5)) {
  //        (size: Int) =>
  //          {
  //            val hoenumBigTrees = constructEnumeratorOtherTypeMemoized
  //            val hoenumSmallTrees = constructEnumeratorOtherTypeMemoized
  //  
  //            // go through all trees of size size
  //            for (blackHeight <- blackHeightRange(size) ) {
  //              val enum = hoenumBigTrees(size, 1 to size, Set(true, false), blackHeight)
  //  
  //              for (ind <- 0 until enum.size) {
  //                enum(ind)
  //              }
  //  
  //              enum shouldBe a [memoization.Memoized[_]]
  //              val mEnum = enum.asInstanceOf[memoization.Memoized[Tree]]
  //              withClue ("The memoized map does have only " +
  //                ( (0 until enum.size).toList forall { (i: Int) => mEnum.isMemoized(enum(i)) } ) +
  //                " memoized, out of " + enum.size) {
  //              }
  //            }
  //  
  //            // membership of all trees up to size size
  //            for (smallerSize <- 1 to size;
  //              blackHeight <- blackHeightRange(smallerSize) ) {
  //                val smallerTreeInput = (smallerSize, 1 to smallerSize, Set(true, false), blackHeight)
  //  
  //                withClue ("For size " + size) {
  //                  withClue ("The memoized map does not have " + smallerTreeInput +
  //                    ".\n The map: " + hoenumBigTrees.memoizedMap.mkString("\n")) {
  //                    ( hoenumBigTrees.isMemoized(smallerTreeInput) ||
  //                      blackHeight == blackHeightRange(smallerSize).end ) shouldBe true
  //                  }
  //                  val correspondingEnum = hoenumBigTrees(smallerTreeInput)
  //                  val enum = hoenumSmallTrees(smallerTreeInput)
  //                  correspondingEnum.size shouldBe enum.size
  //  
  //                  correspondingEnum match {
  //                    case m: memoization.Memoized[_] =>
  //                      enum.size shouldBe m.size
  //  
  //                      m shouldBe a [Map[_, _]]
  //                      withClue ("Total memoized in inner = " +
  //                        ((0 until enum.size) count { (i: Int) => m.isMemoized(enum(i)) })
  //                      ) {
  //                          for (ind <- 0 until enum.size) {
  //                            val currentTreeThatShouldBeMemoized = enum(ind)
  //                            withClue ("Tree " + currentTreeThatShouldBeMemoized + " not memoized.") {
  //                              ( m.isMemoized(currentTreeThatShouldBeMemoized) || {
  //                                val bools =
  //                                  for (innerBlackHeight <- blackHeightRange(size);
  //                                    innerEnum = hoenumBigTrees(size, 1 to size, Set(true, false), innerBlackHeight);
  //                                    i <- 0 until innerEnum.size) yield
  //                                      innerEnum(i).hasSubtree(currentTreeThatShouldBeMemoized)
  //  
  //                                ! bools.exists( identity )
  //                                }
  //                              ) shouldBe true
  //                            }
  //                        }
  //                      }
  //                      info("%d/%d".format( ((0 until enum.size) count { (i: Int) => m.isMemoized(enum(i)) }),
  //                        enum.size))
  //                    case _: Empty[_] =>
  //                  }
  //                }
  //  
  //            }
  //          }
  //      }
  //  
  //    }

  ignore("The times in this test are not dominated by invariant checking") {
    test("Comparison of times for testing after insertion") {

      val normalStopWatch = new Stopwatch("checking invariant cummulative time")
      val memberStopWatch = new Stopwatch("checking membership cummulative time")

      val dependEnumNormal = constructEnumeratorOtherTypeMemoized
      val dependEnumMember = constructEnumeratorOtherTypeMemoized

      forAll(Gen.choose(3, 7), minSuccessful(10)) {
        (size: Int) =>
          {
            for (blackHeight <- blackHeightRange(size)) {

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

      //    println("m: " + memberStopWatch.acc + "n: " + normalStopWatch.acc)
      memberStopWatch.acc should be < normalStopWatch.acc
    }
  }

  test("Comparison of times for testing after insertion, all enumerate first") {

    val normalStopWatch = new Stopwatch("checking invariant cummulative time")
    val memberStopWatch = new Stopwatch("checking membership cummulative time")

    val dependEnumNormal = constructEnumeratorOtherTypeMemoized
    val dependEnumMember = constructEnumeratorOtherTypeMemoized

    for (size <- 7 to 7) {
      // compute whole enum for member
      
      for (blackHeight <- blackHeightRange(size)) {
        val enum = dependEnumMember(size - 1, 1 to size, Set(true, false), blackHeight)

        for (ind <- 0 until enum.size) enum(ind)
      }
      
      for (blackHeight <- blackHeightRange(size)) {

        {
            val enum = dependEnumMember(size - 1, 1 to size, Set(true, false), blackHeight)

            for (ind <- 0 until enum.size) {
              val missing = (1 to size).toList.find(!enum(ind).contains(_))

                val newTree = enum(ind) insert missing.get

          memberStopWatch.profile {
//              profile("Member:") {
              var invariant = false
              for (bH <- blackHeight to (blackHeight + 1); if !invariant) {
                val enumBigger = dependEnumMember(size, 1 to size, Set(true, false), bH)

                invariant = (enumBigger.member(newTree))
              }
              assert(invariant)
              }
//            }
          }
        }
        {
          
          val enum = 
          {
            val enum = dependEnumNormal(size - 1, 1 to size, Set(true, false), blackHeight)
            for (ind <- 0 until enum.size) enum(ind)
            enum
          }

            for (ind <- 0 until enum.size) {
              val missing = (1 to size).toList.find(!enum(ind).contains(_))

              val newTree = enum(ind) insert missing.get

          normalStopWatch.profile {
//            profile("Normal:") {
              assert(RedBlackTrees.size(newTree) == size)
              assert(invariant(newTree))
            }
            }
//          }
        }

      }
    }

    info("m: " + memberStopWatch.acc + "n: " + normalStopWatch.acc)
    memberStopWatch.acc should be < normalStopWatch.acc
  }
  
//  test("figure out what is going on") {
//
//    val dependEnumNormal = constructEnumeratorOtherTypeMemoized
//    val dependEnumMember = constructEnumeratorOtherTypeMemoized
//
//    for (size <- 7 to 7) {
//      // compute whole enum for member
//      
//      for (blackHeight <- blackHeightRange(size)) {
//        val enum = dependEnumMember(size - 1, 1 to size, Set(true, false), blackHeight)
//
//        for (ind <- 0 until enum.size) enum(ind)
//      }
//      
//      for (blackHeight <- blackHeightRange(size)) {
//        {
//            val enum = dependEnumMember(size - 1, 1 to size, Set(true, false), blackHeight)
//
//            for (ind <- 0 to 0) {
//              val missing = (1 to size).toList.find(!enum(ind).contains(_))
//
//                val newTree = enum(ind) insert missing.get
//
//              var invariant = false
//              for (bH <- blackHeight to (blackHeight + 1); if !invariant) {
//                val enumBigger = dependEnumMember(size, 1 to size, Set(true, false), bH)
//
//                invariant = (enumBigger.member(newTree))
//              }
//              assert(invariant)
//              }
//        }
//
//      }
//    }
//  }
  //
  // test("Member after insertion") {
  //    
  //    scala.io.StdIn.readLine
  //
  //    val dependEnumMember = constructEnumeratorOtherTypeMemoized
  //
  //    forAll(Gen.choose(3, 7), minSuccessful(10)) {
  //      (size: Int) =>
  //        {
  //          for (blackHeight <- blackHeightRange(size)) {
  //
  //            val enum = dependEnumMember(size - 1, 1 to size, Set(true, false), blackHeight)
  //
  //            for (ind <- 0 until enum.size) {
  //              val missing = (1 to size).toList.find(!enum(ind).contains(_))
  //              assert(!missing.isEmpty)
  //
  //              val newTree = enum(ind) insert missing.get
  //
  //              withClue("Old tree: %s; New tree: %s".format(enum(ind), newTree)) {
  //                (blackHeight to (blackHeight + 1)).toList.exists { bH: Int =>
  //                  val enumBigger = dependEnumMember(size, 1 to size, Set(true, false), bH)
  //
  //                  enumBigger.member(newTree)
  //                } shouldBe true
  //              }
  //            }
  //
  //          }
  //        }
  //    }
  //
  //  }

  //  test("Comparison of times for testing after insertion") {
  //
  //    val normalStopWatch = new Stopwatch("checking invariant cummulative time")
  //    val memberStopWatch = new Stopwatch("checking membership cummulative time")
  //
  //    val dependEnumNormal = constructEnumeratorOtherTypeMemoized
  //    val dependEnumMember = constructEnumeratorOtherTypeMemoized
  //
  //    forAll(Gen.choose(3, 8), minSuccessful(30)) {
  //      (size: Int) =>
  //        {
  //          for (blackHeight <- blackHeightRange(size)) {
  //
  //            profile("Normal:") {
  //              normalStopWatch.profile {
  //                val enum = dependEnumNormal(size - 1, 1 to size, Set(true, false), blackHeight)
  //
  //                for (ind <- 0 until enum.size;
  //                  missing <- 1 to size) {
  //
  //                  val newTree = enum(ind) insert missing
  //
  //                  withClue("Old tree: %s; New tree: %s".format(enum(ind), newTree)) {
  //                    RedBlackTrees.size(newTree) shouldBe size
  //
  //                    invariant(newTree) shouldBe true
  //                  }
  //                }
  //              }
  //            }
  //
  //            profile("Member:") {
  //              memberStopWatch.profile {
  //                val enum = dependEnumMember(size - 1, 1 to size, Set(true, false), blackHeight)
  //
  //                for (ind <- 0 until enum.size;
  //                  missing <- 1 to size) {
  //
  //                  val newTree = enum(ind) insert missing
  //
  //                  withClue("Old tree: %s; New tree: %s".format(enum(ind), newTree)) {
  //                    (blackHeight to (blackHeight + 1)).toList.exists { bH: Int =>
  //                      val enumBigger = dependEnumMember(size, 1 to size, Set(true, false), bH)
  //
  //                      enumBigger.member(newTree)
  //                    } shouldBe true
  //                  }
  //                }
  //              }
  //            }
  //
  //          }
  //        }
  //    }
  //
  //    //    println("m: " + memberStopWatch.acc + "n: " + normalStopWatch.acc)
  //    memberStopWatch.acc should be < normalStopWatch.acc
  //  }
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
  //  // constructs enumerator for "simple" red-black trees
  //  def constructEnumerator = {
  //
  //    val colorsProducer = new WrapFunctionFin(
  //      (set: Set[Boolean]) => { new WrapArray(set.toArray) })
  //
  //    val treesOfSize = new WrapFunctionFin(
  //      (self: MemberDependFinite[(Int, Range, Set[Boolean], Int), Tree], pair: (Int, Range, Set[Boolean], Int)) => {
  //        val (size, range, colors, blackHeight) = pair
  //
  //        if (range.size >= size && range.size < 0 || blackHeight < 0) new Empty: MemberFinite[Tree]
  //        else if (size == 0 && blackHeight == 1 && colors.contains(true)) new Singleton(Leaf): MemberFinite[Tree]
  //        else if (size > 0 && blackHeight >= 1) {
  //          val roots = new WrapRange(range)
  //          val leftSizes = new WrapArray(0 until size toArray)
  //          val rootColors = colorsProducer(colors)
  //
  //          val rootLeftSizePairs = new member.ProductFinite(leftSizes, roots)
  //          val rootLeftSizeColorTuples = new member.ProductFinite(rootLeftSizePairs, rootColors)
  //
  //          val leftTrees = new InMapFin(self, { (par: ((Int, Int), Boolean)) =>
  //            val ((leftSize, median), rootColor) = par
  //            val childColors = if (rootColor) Set(true, false) else Set(true)
  //            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
  //            (leftSize, range.start to (median - 1), childColors, childBlackHeight)
  //          })
  //
  //          val rightTrees = new InMapFin(self, { (par: ((Int, Int), Boolean)) =>
  //            val ((leftSize, median), rootColor) = par
  //            val childColors = if (rootColor) Set(true, false) else Set(true)
  //            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
  //            (size - leftSize - 1, (median + 1) to range.end, childColors, childBlackHeight)
  //          })
  //
  //          val leftRightPairs =
  //            Product(leftTrees, rightTrees)
  //
  //          val allNodes = new ChainFinite(rootLeftSizeColorTuples, leftRightPairs)
  //
  //          val makeTree =
  //            (p: (((Int, Int), Boolean), (Tree, Tree))) => {
  //              val (((leftSize, currRoot), rootColor), (leftTree, rightTree)) = p
  //
  //              assert(!(size >= 2 && leftSize == 0 && size - leftSize - 1 == 0))
  //              assert(!(size >= 2 && leftTree == Leaf && rightTree == Leaf))
  //              assert(!(leftSize > 0 && leftTree == Leaf), "leftSize=%d, leftTree=Leaf".format(leftSize))
  //              Node(leftTree, currRoot, rightTree, rootColor)
  //            }
  //
  //          val invertTree = {
  //            (p: Tree) =>
  //              {
  //                val Node(leftTree, currRoot, rightTree, rootColor) = p.asInstanceOf[Node]
  //
  //                (((RedBlackTrees.size(leftTree), currRoot), rootColor), (leftTree, rightTree))
  //              }
  //          }
  //
  //          new Map[(((Int, Int), Boolean), (Tree, Tree)), Tree](allNodes, makeTree, invertTree) with MemberFinite[Tree]: MemberFinite[Tree]
  //        } else new Empty: MemberFinite[Tree]
  //      })
  //
  //    treesOfSize
  //  }
  //
  //  // constructs enumerator for red-black trees with operations
  //  def constructEnumeratorOtherType = {
  //    import RedBlackTreeWithOperations._
  //
  //    val colorsProducer = new WrapFunctionFin(
  //      (set: Set[Boolean]) => { new WrapArray(set.toArray) })
  //
  //    val treesOfSize = new WrapFunctionFin(
  //      (self: MemberDependFinite[(Int, Range, Set[Boolean], Int), Tree], pair: (Int, Range, Set[Boolean], Int)) => {
  //        val (size, range, colors, blackHeight) = pair
  //
  //        if (range.size >= size && range.size < 0 || blackHeight < 0) new Empty: MemberFinite[Tree]
  //        else if (size == 0 && blackHeight == 1 && colors.contains(true)) new Singleton(Leaf): MemberFinite[Tree]
  //        else if (size > 0 && blackHeight >= 1) {
  //          val roots = new WrapRange(range)
  //          val leftSizes = new WrapArray(0 until size toArray)
  //          val rootColors = colorsProducer(colors)
  //
  //          val rootLeftSizePairs = new member.ProductFinite(leftSizes, roots)
  //          val rootLeftSizeColorTuples = new member.ProductFinite(rootLeftSizePairs, rootColors)
  //
  //          val leftTrees = new InMapFin(self, { (par: ((Int, Int), Boolean)) =>
  //            val ((leftSize, median), rootColor) = par
  //            val childColors = if (rootColor) Set(true, false) else Set(true)
  //            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
  //            (leftSize, range.start to (median - 1), childColors, childBlackHeight)
  //          })
  //
  //          val rightTrees = new InMapFin(self, { (par: ((Int, Int), Boolean)) =>
  //            val ((leftSize, median), rootColor) = par
  //            val childColors = if (rootColor) Set(true, false) else Set(true)
  //            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
  //            (size - leftSize - 1, (median + 1) to range.end, childColors, childBlackHeight)
  //          })
  //
  //          val leftRightPairs =
  //            Product(leftTrees, rightTrees)
  //
  //          val allNodes = new ChainFinite(rootLeftSizeColorTuples, leftRightPairs)
  //
  //          val makeTree =
  //            (p: (((Int, Int), Boolean), (Tree, Tree))) => {
  //              val (((leftSize, currRoot), rootColor), (leftTree, rightTree)) = p
  //
  //              assert(!(size >= 2 && leftSize == 0 && size - leftSize - 1 == 0))
  //              assert(!(size >= 2 && leftTree == Leaf && rightTree == Leaf))
  //              assert(!(leftSize > 0 && leftTree == Leaf), "leftSize=%d, leftTree=Leaf".format(leftSize))
  //              Node(rootColor, leftTree, currRoot, rightTree)
  //            }
  //
  //          val invertTree = {
  //            (p: Tree) =>
  //              {
  //                val Node(rootColor, leftTree, currRoot, rightTree) = p.asInstanceOf[Node]
  //
  //                (((RedBlackTrees.size(leftTree), currRoot), rootColor), (leftTree, rightTree))
  //              }
  //          }
  //
  //          new Map[(((Int, Int), Boolean), (Tree, Tree)), Tree](allNodes, makeTree, invertTree) with MemberFinite[Tree]: MemberFinite[Tree]
  //        } else new Empty: MemberFinite[Tree]
  //      })
  //
  //    treesOfSize
  //  }

  def constructEnumeratorOtherTypeMemoized = {
    import RedBlackTreeWithOperations._
    import dependent._

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

          new Map[(((Int, Int), Boolean), (Tree, Tree)), Tree](allNodes, makeTree, invertTree) with member.memoization.Memoized[Tree] with e.memoization.Memoized[Tree] with MemberFinite[Tree]: MemberFinite[Tree]
        } else new Empty: MemberFinite[Tree]
      }) with e.memoization.dependent.Memoized[(Int, Range, Set[Boolean], Int), Tree]

    treesOfSize
  }

  //  def constructEnumeratorOtherTypeMemoizedBlackHeight = {
  //    import RedBlackTreeWithOperations._
  //
  //    val colorsProducer = new WrapFunctionFin(
  //      (set: Set[Boolean]) => { new WrapArray(set.toArray) })
  //
  //    val treesOfSize = new WrapFunctionFin(
  //      (self: MemberDependFinite[(Int, Range, Set[Boolean], Int), Tree], pair: (Int, Range, Set[Boolean], Int)) => {
  //        val (size, range, colors, blackHeight) = pair
  //
  //        if (range.size >= size && range.size < 0 || blackHeight < 0) new Empty: MemberFinite[Tree]
  //        else if (size == 0 && blackHeight == 1 && colors.contains(true)) new Singleton(Leaf): MemberFinite[Tree]
  //        else if (size > 0 && blackHeight >= 1) {
  //          val roots = new WrapRange(range)
  //          val leftSizes = new WrapArray(0 until size toArray)
  //          val rootColors = colorsProducer(colors)
  //
  //          val rootLeftSizePairs = new member.ProductFinite(leftSizes, roots)
  //          val rootLeftSizeColorTuples = new member.ProductFinite(rootLeftSizePairs, rootColors)
  //
  //          val leftTrees = new InMapFin(self, { (par: ((Int, Int), Boolean)) =>
  //            val ((leftSize, median), rootColor) = par
  //            val childColors = if (rootColor) Set(true, false) else Set(true)
  //            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
  //            (leftSize, range.start to (median - 1), childColors, childBlackHeight)
  //          })
  //
  //          val rightTrees = new InMapFin(self, { (par: ((Int, Int), Boolean)) =>
  //            val ((leftSize, median), rootColor) = par
  //            val childColors = if (rootColor) Set(true, false) else Set(true)
  //            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
  //            (size - leftSize - 1, (median + 1) to range.end, childColors, childBlackHeight)
  //          })
  //
  //          val leftRightPairs =
  //            Product(leftTrees, rightTrees)
  //
  //          val allNodes = new ChainFinite(rootLeftSizeColorTuples, leftRightPairs)
  //
  //          val makeTree =
  //            (p: (((Int, Int), Boolean), (Tree, Tree))) => {
  //              val (((leftSize, currRoot), rootColor), (leftTree, rightTree)) = p
  //
  //              assert(!(size >= 2 && leftSize == 0 && size - leftSize - 1 == 0))
  //              assert(!(size >= 2 && leftTree == Leaf && rightTree == Leaf))
  //              assert(!(leftSize > 0 && leftTree == Leaf), "leftSize=%d, leftTree=Leaf".format(leftSize))
  //              Node(rootColor, leftTree, currRoot, rightTree)
  //            }
  //
  //          val invertTree = {
  //            (p: Tree) =>
  //              {
  //                val Node(rootColor, leftTree, currRoot, rightTree) = p.asInstanceOf[Node]
  //
  //                (((RedBlackTrees.size(leftTree), currRoot), rootColor), (leftTree, rightTree))
  //              }
  //          }
  //
  //          new Map[(((Int, Int), Boolean), (Tree, Tree)), Tree](allNodes, makeTree, invertTree) with MemberFinite[Tree] with e.memoization.Memoized[Tree] with Memoized[Tree]: MemberFinite[Tree]
  //        } else new Empty: MemberFinite[Tree]
  //      }) with e.memoization.dependent.Memoized[(Int, Range, Set[Boolean], Int), Tree]
  //
  //    treesOfSize
  //  }
  //
  //  def constructEnumeratorNormal = {
  //    import e.dependent._
  //
  //    val colorsProducer = Depend(
  //      (set: Set[Boolean]) => { e.WrapArray(set.toArray) })
  //
  //    val treesOfSize: Depend[(Int, Range, Set[Boolean], Int), Tree] = Depend(
  //      (self: Depend[(Int, Range, Set[Boolean], Int), Tree], pair: (Int, Range, Set[Boolean], Int)) => {
  //        val (size, range, colors, blackHeight) = pair
  //
  //        if (range.size >= size && range.size < 0 || blackHeight < 0) e.Empty
  //        else if (size == 0 && blackHeight == 1 && colors.contains(true)) e.Singleton(Leaf)
  //        //        else if (size == 1 && blackHeight == 1 && colors.contains(false)) e.WrapArray(range map { v => Node(Leaf, v, Leaf, false) })
  //        //        else if (size == 1 && blackHeight == 2 && colors.contains(true)) e.WrapArray(range map { v => Node(Leaf, v, Leaf, true) })
  //        //        else if (size == 1) e.WrapArray(range map { v => Node(Leaf, v, Leaf, true) })
  //        else if (size > 0 && blackHeight >= 1) {
  //          val roots = e.Enum(range)
  //          val leftSizes = e.WrapArray(0 until size)
  //          val rootColors = colorsProducer(colors)
  //
  //          val rootLeftSizePairs = e.Product(leftSizes, roots)
  //          val rootLeftSizeColorTuples = e.Product(rootLeftSizePairs, rootColors)
  //
  //          val leftTrees: Depend[((Int, Int), Boolean), Tree] = InMap(self, { (par: ((Int, Int), Boolean)) =>
  //            val ((leftSize, median), rootColor) = par
  //            val childColors = if (rootColor) Set(true, false) else Set(true)
  //            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
  //            (leftSize, range.start to (median - 1), childColors, childBlackHeight)
  //          })
  //
  //          val rightTrees: Depend[((Int, Int), Boolean), Tree] = InMap(self, { (par: ((Int, Int), Boolean)) =>
  //            val ((leftSize, median), rootColor) = par
  //            val childColors = if (rootColor) Set(true, false) else Set(true)
  //            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
  //            (size - leftSize - 1, (median + 1) to range.end, childColors, childBlackHeight)
  //          })
  //
  //          val leftRightPairs: Depend[((Int, Int), Boolean), (Tree, Tree)] =
  //            Product(leftTrees, rightTrees)
  //
  //          val allNodes =
  //            Chain[((Int, Int), Boolean), (Tree, Tree), Node](rootLeftSizeColorTuples, leftRightPairs,
  //              (p1: ((Int, Int), Boolean), p2: (Tree, Tree)) => {
  //                val (((leftSize, currRoot), rootColor), (leftTree, rightTree)) = (p1, p2)
  //
  //                assert(!(size >= 2 && leftSize == 0 && size - leftSize - 1 == 0))
  //                assert(!(size >= 2 && leftTree == Leaf && rightTree == Leaf))
  //                assert(!(leftSize > 0 && leftTree == Leaf), "leftSize=%d, leftTree=Leaf".format(leftSize))
  //                Node(leftTree, currRoot, rightTree, rootColor)
  //              })
  //
  //          allNodes
  //        } else e.Empty
  //      })
  //
  //    treesOfSize
  //  }

}
