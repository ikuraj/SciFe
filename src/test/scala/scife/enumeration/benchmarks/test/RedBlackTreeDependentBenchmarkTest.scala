package scife
package enumeration
package benchmarks
package test

import dependent._
import memoization._
import scife.{ enumeration => e }
import scife.util._

import scife.util.logging._

import org.scalatest._
import org.scalatest.prop._
import org.scalameter.api._
import org.scalacheck.Gen

import scala.language.postfixOps
import scala.language.existentials

class RedBlackTreeDependentBenchmarkTest
  extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {

  import e.common.enumdef.RedBlackTreeEnum._
  import structures.RedBlackTrees._
  
  import Util._
  import Checks._
  import Common._

  def checker(seq: Seq[Tree]) {
    seq.distinct.size should be(seq.size)
    seq.forall(blackInv(_)) should be(true)
    seq.forall(redDescHaveBlackChildren(_)) should be(true)
    seq.forall(valueOrdering(_)) should be(true)
    seq.forall(invariant(_)) should be(true)
  }

  test("correctness") {
    val ms = new scope.AccumulatingScope
    val enum = constructEnumeratorBenchmarkTest(ms)
    ms.memoizations.size should be(2)

    val helper = new CheckerHelperFun(checker)
    import helper._

    val profileRange = 1 to 6

    withLazyClue("Elements are: " + clue) {
      // specific cases
      elements = enum.getEnum((1, 1 to 1, Set(true), 2)).toList
      elements.size should be(1)
      elements = enum.getEnum((2, 1 to 2, Set(true), 2)).toList
      elements.size should be(2)
      elements = enum.getEnum((4, 1 to 4, Set(true, false), 2)).toList
      elements.size should be(4)

      elements = enum.getEnum((3, 1 to 3, Set(true, false), 2)).toList
      elements.size should be(2)
      elements = enum.getEnum((3, 1 to 3, Set(true, false), 3)).toList
      elements.size should be(1)

      for (size <- profileRange) {
        info("Generating for size " + size)
        elements =
          for (
            blackHeight <- 0 to (size + 1); e = enum.getEnum(size, 1 to size, Set(true, false), blackHeight);
            ind <- 0 until e.size
          ) yield e(ind)

        elements.forall(invariant(_)) should be(true)

        profile("Claculating size %d".format(size)) {
          elements.size should be(numberOfTrees(size))
        }
      }

      // logged
      for (size <- profileRange) {
        info("Generating for size " + size)
        elements =
          for (
            blackHeight <- 1 to (Math.log2(size + 1).toInt + 1); e = enum.getEnum(size, 1 to size, Set(true, false), blackHeight);
            ind <- 0 until e.size
          ) yield e(ind)

        elements.forall(invariant(_)) should be(true)

        profile("Claculating size %d".format(size)) {
          elements.size should be(numberOfTrees(size))
        }
      }

      // some confirmed counts
      elements =
        for (
          blackHeight <- 0 to 6; e = enum.getEnum(9, 1 to 9, Set(true, false), blackHeight);
          ind <- 0 until e.size
        ) yield e(ind)
      elements.size should be(122)
    }

    for (size <- profileRange) {
      ms.clear
      profile("Getting enums and elements for RBT of size %d".format(size)) {
        elements =
          for (
            blackHeight <- 0 to (size + 1); e = enum.getEnum(size, 1 to size, Set(true, false), blackHeight);
            ind <- 0 until e.size
          ) yield e(ind)
      }
    }

    for (size <- profileRange) {
      ms.clear
      profile("Getting enums and elements for RBT of size %d".format(size)) {
        elements =
          for (
            blackHeight <- 1 to (Math.log2(size + 1).toInt + 1); e = enum.getEnum(size, 1 to size, Set(true, false), blackHeight);
            ind <- 0 until e.size
          ) yield e(ind)
      }

      assert((for (el <- elements) yield el).forall(invariant(_)))
    }

  }

  test("correct black heights") {
    val ms = new scope.AccumulatingScope
    val hoenum = constructEnumeratorBenchmarkTest(ms)

    forAll(Gen.choose(1, 10), minSuccessful(30)) {
      (size: Int) =>
        {
          val trees1 =
            for (
              blackHeight <- 0 to (size + 1);
              e = hoenum.getEnum(size, 1 to size, Set(true, false), blackHeight)
            ) yield e.size

          val trees2 =
            for (
              blackHeight <- 1 to (Math.log2(size + 1).toInt + 1);
              e = hoenum.getEnum(size, 1 to size, Set(true, false), blackHeight)
            ) yield e.size

          trees1.sum shouldEqual trees2.sum

          val trees3 =
            for (
              blackHeight <- Math.log2(size).toInt to (Math.log2(size + 1).toInt + 1);
              e = hoenum.getEnum(size, 1 to size, Set(true, false), blackHeight)
            ) yield e.size

          withClue("around logarithm") {
            trees1.sum shouldEqual trees3.sum
          }

        }
    }
  }

}
