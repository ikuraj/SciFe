package scife
package enumeration
package member
package testcase

import scife.{ enumeration => e }
import dependent._
import scife.util.logging._
import scife.util._
import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen
import scife.enumeration.dependent.Depend

class BinarySearchTreeFromSetTryTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {
  import Checks._
  import scife.util.structures._
  import bst.ybanez._
  import BST._
  
  type Input = (Int, Array[Int])

  type Tree = BST[Int]
  type Node = NonEmptyBST[Int]
  val Leaf = EmptyBST

  import scala.language.implicitConversions
  implicit def thisBST2simpleBST(tree: BST[Int]): BSTrees.Tree = BST.thisBST2simpleBST(tree)

  ignore("Comparison of times for testing after insertion, all enumerate first") {

    val normalStopWatch = new Stopwatch("checking invariant cummulative time")
    val memberStopWatch = new Stopwatch("checking membership cummulative time")

    val dependEnumNormal = constructEnumerator
    val dependEnumMember = constructEnumerator

    for (size <- 1 to 7; depend <- dependEnumMember :: dependEnumNormal :: Nil) {
      val enum = depend( (size, Array.range(1, size+1)) )
      for (ind <- 0 until enum.size) yield enum(ind)
    }

    var flag = 0

    for (size <- 1 to 6) {

      {
        val enum = dependEnumNormal(size - 1, Array.range(1, size+1))

        for (ind <- 0 until enum.size) {
          val missing = (1 to size).toList.find(!enum(ind).contains(_))

          val newTree = enum(ind) + missing.get

          normalStopWatch.profile {
            if (newTree.size == size) flag += 1
            if (BSTrees.invariant(newTree)) flag += 1
            if (BSTrees.valuesInRange(newTree, 1, size)) flag += 1
          }
        }
      }

      {
        val enum = dependEnumMember(size - 1, Array.range(1, size+1))
        val memberEnum = 
          memberStopWatch.profile { dependEnumMember(size, Array.range(1, size+1)) }

        for (ind <- 0 until enum.size) {
          val missing = (1 to size).toList.find(!enum(ind).contains(_))

          val newTree = enum(ind) + missing.get

          memberStopWatch.profile {
            if (!memberEnum.member(newTree)) flag += 1
          }
        }
      }

    }

    info(flag.toString)
    info("m: " + memberStopWatch.acc + "n: " + normalStopWatch.acc)
    memberStopWatch.acc should be < normalStopWatch.acc
  }

  ignore("debugging test") {

    val normalStopWatch = new Stopwatch
    val memberStopWatch = new Stopwatch

    val dependMember = constructEnumerator
    val dependNormal = constructEnumeratorNormal

    val allElements =
      for (
        size <- 1 to 7;
        ind <- 0 until dependMember(size, Array.range(1, size+1)).size
      ) yield (size, dependMember(size, Array.range(1, size+1))(ind))

//    print("PROFIIIIIILE")
//    while (true) {
//      for ((size, el) <- allElements)
//        dependMember(size, Array.range(1, size+1)).member(el)
//    }

    for (
      size <- 1 to 7;
      (depend, stopwatch) <- dependNormal :: dependMember :: Nil zip
        (normalStopWatch :: memberStopWatch :: Nil)
    ) {
      info(s"size =$size")

      stopwatch.profile {
        val enum = depend(size, Array.range(1, size+1))
        for (ind <- 0 until enum.size) enum(ind)
      }

    }

    info(s"Member enumerator: ${memberStopWatch.acc}ms, normal enumerator: ${normalStopWatch.acc}ms")
  }

  test("test membership of newly created trees") {

    val depend = constructEnumerator

    for (size <- 1 to 6) {
      info(s"size =$size")
      val wholeCollection = {
        val enum = depend(size, Array.range(1, size+1))
        for (ind <- 0 until enum.size) yield enum(ind)
      }

      val enum = depend(size - 1, Array.range(1, size+1))
      info(s"for (size, range)=${(size - 1, 1 to size+1)}, enum.size = $enum.size")
      for (ind <- 0 until enum.size) {
        val missing = (1 to size).toList.find(!enum(ind).contains(_))
        val newTree = enum(ind) + missing.get

        wholeCollection should contain(newTree)
        info(s"checking membership for $newTree")
        assert(depend(size, Array.range(1, size+1)).member(newTree))
      }

    }
  }

  test("correctness of enumeration") {

    common.BinarySearchTreeTest.testCorrectness(Depend.fin {
      in: (Int, Range) =>
        constructEnumerator(in._1, in._2.toArray) map { x => BST.thisBST2simpleBST(x) }
    })

  }

  test("member recognition") {
    val trees = constructEnumerator

    {
      val en = trees.getEnum(1, Array(1, 2)): Member[Tree]
      for ((revEl, ind) <- List(Node(1), Node(2)).zipWithIndex) {
        en.member(revEl) should be(true)
      }
    }

    val normalTrees = constructEnumeratorNormal

    forAll(Gen.choose(1, 5), Gen.choose(1, 5), minSuccessful(20)) {
      (size: Int, m: Int) =>
        {
          val normalList = normalTrees.getEnum(size, Array.range(1, m+1))

          for (ind <- 0 until normalList.size) {
            trees(size, Array.range(1, m+1)).member(normalList(ind)) should be(true)
          }

        }
    }
  }

  private def constructEnumerator = {
    new WrapFunctionFin(
      (self: MemberDependFinite[Input, Tree], pair: Input) => {
        val (size, range) = pair

        if (size <= 0) new Singleton(Leaf): MemberFinite[Tree]
        else if (size == 1) (
          ((new WrapArray(range)): MemberFinite[Int]).map[Tree](
            { (v: Int) => Node(Leaf, v, Leaf): Tree },
            { (t: Tree) => t.asInstanceOf[Node].elem }
          )
          ): MemberFinite[Tree]
        else {
          val roots = new WrapRange(0 until range.size)
          val leftSizes = new WrapRange(0 until size)
          
//          val rootsWithSplits = roots map {
//            (ind: Int) => (range.take(ind), range(ind), range.takeRight(range.size - ind - 1))
//          }

          val rootLeftSizePairs = new member.ProductFinite(leftSizes, roots)

          val leftTrees = new InMapFin(self, { (par: (Int, Int)) =>
            val (leftSize, medianInd) = par
            (leftSize, range.take(medianInd))
          })

          val rightTrees =
            new InMapFin(self, { (par: (Int, Int)) =>
              val (leftSize, medianInd) = par
              (size - leftSize - 1, range takeRight (range.size - medianInd - 1))
            })

          val leftRightPairs =
            //new ProductFinite(leftTrees, rightTrees)
            Product(leftTrees, rightTrees)

          val allNodes =
            new eager.ChainFinite(rootLeftSizePairs, leftRightPairs): MemberFinite[((Int, Int), (Tree, Tree))]

          val makeTree =
            (p: ((Int, Int), (Tree, Tree))) => {
              val ((leftSize, currRootInd), (leftTree, rightTree)) = p

              Node(leftTree, range(currRootInd), rightTree)
            }

          val memberTree =
            (t: Tree) => {
              val n = t.asInstanceOf[Node]
              val leftSize = n.left.size
              val currRootInd = range indexOf n.elem
              val leftTree = n.left
              val rightTree = n.right

              ((leftSize, currRootInd), (leftTree, rightTree))
            }

          new Map[((Int, Int), (Tree, Tree)), Tree](allNodes, makeTree, memberTree) with MemberFinite[Tree] with e.memoization.Memoized[Tree] with member.memoization.Memoized[Tree]: MemberFinite[Tree]
        }
      }) with e.dependent.DependFinite[Input, Tree] with e.memoization.dependent.Memoized[Input, Tree]
  }
  
  private def constructEnumeratorNormal = {
    import e._
    import e.dependent.{ Product => DepProduct, _ }
    
    new WrapFunctionFin(
      (self: DependFinite[Input, Tree], pair: Input) => {
        val (size, range) = pair

        if (size <= 0) new Singleton(Leaf): Finite[Tree]
        else if (size == 1) new WrapArray(range map { v => Node(Leaf, v, Leaf) }): Finite[Tree]
        else {
          val roots = new WrapRange(0 until range.size)
          val leftSizes = new WrapRange(0 until size)
          
//          val rootsWithSplits = roots map {
//            (ind: Int) => (range.take(ind), range(ind), range.takeRight(range.size - ind - 1))
//          }

          val rootLeftSizePairs = new e.lzy.ProductFinite(leftSizes, roots)

          val leftTrees = InMap(self, { (par: (Int, Int)) =>
            val (leftSize, medianInd) = par
            (leftSize, range.take(medianInd))
          })

          val rightTrees =
            InMap(self, { (par: (Int, Int)) =>
              val (leftSize, medianInd) = par
              (size - leftSize - 1, range takeRight (range.size - medianInd - 1))
            })

          val leftRightPairs =
            //new ProductFinite(leftTrees, rightTrees)
            DepProduct(leftTrees, rightTrees)

          val allNodes =
            new ChainFinite(rootLeftSizePairs, leftRightPairs): Finite[((Int, Int), (Tree, Tree))]

          val makeTree =
            (p: ((Int, Int), (Tree, Tree))) => {
              val ((leftSize, currRootInd), (leftTree, rightTree)) = p

              Node(leftTree, range(currRootInd), rightTree)
            }

          new Map[((Int, Int), (Tree, Tree)), Tree](allNodes, makeTree) with Finite[Tree] with e.memoization.Memoized[Tree]: Finite[Tree]
        }
      }) with e.dependent.DependFinite[Input, Tree] with e.memoization.dependent.Memoized[Input, Tree]
  }

}
