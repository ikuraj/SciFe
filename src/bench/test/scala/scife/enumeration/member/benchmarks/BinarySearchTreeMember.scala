package scife
package enumeration
package member
package benchmarks

import scife.{ enumeration => e }
import dependent._
import e.memoization._

import scife.enumeration.benchmarks._

import org.scalameter.api._

class BinarySearchTreeMember
  extends DependentMemoizedBenchmark[
    (Int, Int),
    MemberDependFinite[(Int, Range), scife.util.structures.bst.ybanez.BST[Int]]] {
  
  fixture("BinarySearchTreeMember", "SciFe", 8)

  import scife.util.structures._
  import bst.ybanez._
  import BST._
  
  type BenchmarkInput = (Int, Int)

  type Tree = BST[Int]
  type Node = NonEmptyBST[Int]
  val Leaf = EmptyBST
  
  type EnumType = MemberDependFinite[(Int, Range), Tree]
  
  override def generator(maxSize: Int): Gen[(Int, Int)] =
    for (size <- Gen.range("size")(1, maxSize, 1);
      missingEl <- Gen.range("missingElement")(0, size - 1, 1)) yield
      (size, missingEl)

  var toVerifyBigger: Traversable[Tree] = _  
  // same is trivially checked with equality
//  var toVerifySame: Traversable[Tree] = _  
    
  def measureCode(tdEnum: EnumType) = {
    { (in: BenchmarkInput) =>
      val (size, _) = in
      val enum = tdEnum.getEnum((size, 1 to size))
      for (tree <- toVerifyBigger) enum.member(tree)
    }
  }

  override def setUp(in: BenchmarkInput, tdEnum: EnumType, memScope: MemoizationScope) {
    val (size, missInd) = in
    
    val values = for ( (v, ind) <- 1 to size zipWithIndex; if ind != missInd) yield v
    val missEl = 1 to size apply missInd

    val enum = tdEnum( size-1, 1 to size-1 )
    
    setValues(values)
//    toVerifySame = for (el <- enum; toInsert <- values) yield el insert toInsert 
    toVerifyBigger = for (el <- enum) yield el + missEl 
    setValues(1 to size)
//    missingElements =
//      for (
//        range <- 1 to maxSize+1;
//        size <- 1 to maxSize+1;
//        enum = inEnum.getEnum((size, 1 to range));
//        i <- 0 until enum.size;
//        orgTree = enum(i);
//        list <- Sublists(1 to range toList);
////        missing = (1 to size).toList.find(!enum(ind).contains(_));
//        newTree = (orgTree /: list) { (tree, el) => tree + el };
//        if orgTree != newTree
//      ) yield (s, el, newTree)
  }

  def warmUp(inEnum: EnumType, maxSize: Int) {
    for (size <- 1 to maxSize) {
      val enum = inEnum.getEnum((size, 1 to size))
      for (i <- 0 until enum.size) enum(i)
    }
  }

  def setValues(inValues: Traversable[Int]) {
    values = inValues.toArray
    mapFromValues = inValues.toList.zipWithIndex.toMap
  }
  
  var values: Array[Int] = _
  var mapFromValues: scala.collection.Map[Int, Int] = _

  def constructEnumerator(implicit ms: MemoizationScope) = {
    val rootProducer = new WrapFunctionFin(
      (range: Range) => { new WrapRange(range) })

    val sizeProducer = new WrapFunctionFin(
      (size: Int) => { new WrapRange(0 until size) })

    new WrapFunctionFin(
      (self: MemberDependFinite[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        if (size <= 0) new Singleton(Leaf): MemberFinite[Tree]
//        else if (size == 1) new WrapArray(Array(range map { i => Node(Leaf, values(i-1), Leaf) }: _*)): MemberFinite[Tree]
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
              val currRoot = mapFromValues(n.elem)+1
              val leftTree = n.left
              val rightTree = n.right

              ((leftSize, currRoot), (leftTree, rightTree))
            }

          new Map[((Int, Int), (Tree, Tree)), Tree](allNodes, makeTree, memberTree) with MemberFinite[Tree] with e.memoization.Memoized[Tree] with member.memoization.Memoized[Tree]: MemberFinite[Tree]
        }
      }) with e.dependent.DependFinite[(Int, Range), Tree] with e.memoization.dependent.Memoized[(Int, Range), Tree]
  }

}
