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

class BinarySearchTreeExtendedTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {

  import Checks._
  import scife.util.structures._
  import bst.ybanez._
  import BST._

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

    for (size <- 1 to 10; depend <- dependEnumMember :: dependEnumNormal :: Nil) {
      val enum = depend(size, 1 to size)
      for (ind <- 0 until enum.size) yield enum(ind)
    }

    var flag = 0

    for (size <- 1 to 9) {

      {
        val enum = dependEnumNormal(size - 1, 1 to size)

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
        val enum = dependEnumMember(size - 1, 1 to size)
        val memberEnum = 
          memberStopWatch.profile { dependEnumMember(size, 1 to size) }

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
    val almostNormalStopWatch = new Stopwatch

    val dependMember = constructEnumerator
    val dependNormal = constructEnumeratorNormal
    val dependAlmostNormal = constructEnumeratorAlmostNormal

    val allElements =
      for (
        size <- 1 to 10;
        ind <- 0 until dependMember(size, 1 to size).size
      ) yield (size, dependMember(size, 1 to size)(ind))

    print("PROFIIIIIILE")
    while (true) {
      for ((size, el) <- allElements)
        dependMember(size, 1 to size).member(el)
    }

    for (
      size <- 1 to 6;
      (depend, stopwatch) <- dependAlmostNormal :: dependNormal :: dependMember :: Nil zip
        (almostNormalStopWatch :: normalStopWatch :: memberStopWatch :: Nil)
    ) {
      info(s"size =$size")

      stopwatch.profile {
        val enum = depend(size, 1 to size)
        for (ind <- 0 until enum.size) enum(ind)
      }

    }

    info(s"Member enumerator: ${memberStopWatch.acc}ms, normal enumerator: ${normalStopWatch.acc}ms," +
      s"almost normal enumerator: ${almostNormalStopWatch.acc}ms")
  }

  test("test membership of newly created trees") {

    val depend = constructEnumerator

    for (size <- 1 to 7) {
      info(s"size =$size")
      val wholeCollection = {
        val enum = depend(size, 1 to size)
        for (ind <- 0 until enum.size) yield enum(ind)
      }

      val enum = depend(size - 1, 1 to size)
      info(s"for (size, range)=${(size - 1, 1 to size)}, enum.size = $enum.size")
      for (ind <- 0 until enum.size) {
        val missing = (1 to size).toList.find(!enum(ind).contains(_))
        val newTree = enum(ind) + missing.get

        wholeCollection should contain(newTree)
        assert(depend(size, 1 to size).member(newTree))
      }

    }
  }

  test("correctness of enumeration") {

    common.BinarySearchTreeTestHelper.testCorrectness(Depend.fin {
      in: (Int, Range) =>
        constructEnumerator(in) map { x => BST.thisBST2simpleBST(x) }
    })

  }

  test("member recognition") {
    val trees = constructEnumerator

    {
      val en = trees.getEnum(1, 1 to 2): Member[Tree]
      for ((revEl, ind) <- List(Node(1), Node(2)).zipWithIndex) {
        en.member(revEl) should be(true)
      }
    }

    val normalTrees = constructEnumeratorNormal

    forAll(Gen.choose(1, 5), Gen.choose(1, 5), minSuccessful(20)) {
      (size: Int, m: Int) =>
        {
          val normalList = normalTrees.getEnum(size, 1 to m)

          for (ind <- 0 until normalList.size) {
            trees(size, 1 to m).member(normalList(ind)) should be(true)
          }

        }
    }
  }

  private def constructEnumerator = {
    val rootProducer = new WrapFunctionFin(
      (range: Range) => { new WrapRange(range) })

    val sizeProducer = new WrapFunctionFin(
      (size: Int) => { new WrapRange(0 until size) })

    new WrapFunctionFin(
      (self: MemberDependFinite[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        if (size <= 0) new Singleton(Leaf): MemberFinite[Tree]
        else if (size == 1) new WrapArray(Array(range map { v => Node(Leaf, v, Leaf) }: _*)): MemberFinite[Tree]
        else {
          val roots = rootProducer.getEnum(range)
          val leftSizes = sizeProducer.getEnum(size)

          val rootLeftSizePairs = new member.ProductFinite(leftSizes, roots)

          val leftTrees = new InMapFin(self, { (par: (Int, Int)) =>
            val (leftSize, median) = par
            (leftSize, range.start to (median - 1))
          })

          val rightTrees =
            new InMapFin(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (size - leftSize - 1, (median + 1) to range.end)
            })

          val leftRightPairs =
            //new ProductFinite(leftTrees, rightTrees)
            Product(leftTrees, rightTrees)

          val allNodes =
            new eager.ChainFinite(rootLeftSizePairs, leftRightPairs): MemberFinite[((Int, Int), (Tree, Tree))]

          val makeTree =
            (p: ((Int, Int), (Tree, Tree))) => {
              val ((leftSize, currRoot), (leftTree, rightTree)) = p

              Node(leftTree, currRoot, rightTree)
            }

          val memberTree =
            (t: Tree) => {
              val n = t.asInstanceOf[Node]
              val leftSize = n.left.size
              val currRoot = n.elem
              val leftTree = n.left
              val rightTree = n.right

              ((leftSize, currRoot), (leftTree, rightTree))
            }

          new {
            override val classTagT = implicitly[scala.reflect.ClassTag[Tree]]
          } with Map[((Int, Int), (Tree, Tree)), Tree](allNodes, makeTree, memberTree) with MemberFinite[Tree] with
            e.memoization.MemoizedSize with e.memoization.MemoizedStatic[Tree] with member.memoization.Memoized[Tree]: MemberFinite[Tree]
        }
      }) with e.dependent.DependFinite[(Int, Range), Tree] with e.memoization.dependent.Memoized[(Int, Range), Tree]
  }

  trait DummyMember[T] extends Member[T] {
    override def member(t: T) = false
  }

  private def constructEnumeratorAlmostNormal = {
    val rootProducer = new WrapFunctionFin(
      (range: Range) => { new WrapRange(range) })

    val sizeProducer = new WrapFunctionFin(
      (size: Int) => { new WrapRange(0 until size) })

    //    new e.dependent.WrapFunctionFin(
    new WrapFunctionFin(
      //      (self: e.dependent.DependFinite[(Int, Range), Tree], pair: (Int, Range)) => {
      (self: MemberDependFinite[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        if (size <= 0) new Singleton(Leaf): MemberFinite[Tree]
        else if (size == 1) new WrapArray(Array(range map { v => Node(Leaf, v, Leaf) }: _*)): MemberFinite[Tree]
        else {
          val roots = rootProducer.getEnum(range)
          val leftSizes = sizeProducer.getEnum(size)

          val rootLeftSizePairs = new member.ProductFinite(leftSizes, roots)

          val leftTrees =
            new InMapFin(self, { (par: (Int, Int)) =>
              //          e.dependent.InMap(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (leftSize, range.start to (median - 1))
            })

          val rightTrees =
            new InMapFin(self, { (par: (Int, Int)) =>
              //            e.dependent.InMap(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (size - leftSize - 1, (median + 1) to range.end)
            })

          val leftRightPairs =
            //new ProductFinite(leftTrees, rightTrees)
            Product(leftTrees, rightTrees)

          val allNodes =
            new eager.ChainFinite(rootLeftSizePairs, leftRightPairs): MemberFinite[((Int, Int), (Tree, Tree))]
          //            new e.dependent.ChainFinite(rootLeftSizePairs, leftRightPairs)

          val makeTree =
            (p: ((Int, Int), (Tree, Tree))) => {
              val ((leftSize, currRoot), (leftTree, rightTree)) = p

              Node(leftTree, currRoot, rightTree)
            }

          val memberTree =
            (t: Tree) => {
              val n = t.asInstanceOf[Node]
              val leftSize = n.left.size
              val currRoot = n.elem
              val leftTree = n.left
              val rightTree = n.right

              ((leftSize, currRoot), (leftTree, rightTree))
            }

          new {
            override val classTagT = implicitly[scala.reflect.ClassTag[Tree]]
          } with e.Map[((Int, Int), (Tree, Tree)), Tree](allNodes, makeTree) with DummyMember[Tree] with MemberFinite[Tree] with
            e.memoization.MemoizedSize with e.memoization.MemoizedStatic[Tree] with member.memoization.Memoized[Tree]: MemberFinite[Tree]
          //          allNodes map makeTree: Finite[Tree]
        }
      }) with e.dependent.DependFinite[(Int, Range), Tree] with e.memoization.dependent.Memoized[(Int, Range), Tree]
  }

  private def constructEnumeratorNormal = {
    import e.dependent._

    val rootProducer = new WrapFunctionFin(
      (range: Range) => { new WrapRange(range) })

    val sizeProducer = new WrapFunctionFin(
      (size: Int) => {
        Enum(0 until size)
      })

    Depend.memoizedFin(
      (self: e.dependent.DependFinite[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        if (size <= 0) Enum(Leaf): Finite[Tree]
        else if (size == 1) Enum(range.toArray map { v => Node(Leaf, v, Leaf) }): Finite[Tree]
        else {
          val roots = rootProducer.getEnum(range)
          val leftSizes = sizeProducer.getEnum(size)

          val rootLeftSizePairs = //e.Product(leftSizes, roots)
            new e.lzy.ProductFinite(leftSizes, roots)

          val leftTrees = InMap(self,
            { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (leftSize, range.start to (median - 1))
            })

          val rightTrees =
            InMap(self,
              { (par: (Int, Int)) =>
                val (leftSize, median) = par
                (size - leftSize - 1, (median + 1) to range.end)
              })

          val leftRightPairs =
            Product(leftTrees, rightTrees)

          val allNodes =
            new ChainFinite(rootLeftSizePairs, leftRightPairs)

          val makeTree =
            (p: ((Int, Int), (Tree, Tree))) => {
              val ((leftSize, currRoot), (leftTree, rightTree)) = p

              Node(leftTree, currRoot, rightTree)
            }

          allNodes map makeTree: Finite[Tree]
        }
      })
  }

}
