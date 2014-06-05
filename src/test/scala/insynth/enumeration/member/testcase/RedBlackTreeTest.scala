package insynth.enumeration
package member
package testcase

import insynth.{ enumeration => e }
import dependent._

import util._
import insynth.util.logging._
import insynth.util._

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

import scala.language.postfixOps

class RedBlackTreeTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
	HasLogger with ProfileLogger {  
  import Checks._
  import Structures._
  import RedBlackTrees._
  
  test("regular enumeration") {
    
    common.RedBlackTreeDependentTest.testCorrectness( constructEnumerator )

  }
  
//  test("member enumeration") {
//    val trees = constructEnumerator
//    
//    {
//      val en = trees.getEnum(1, 1 to 2): Member[Tree]
//      for ((revEl, ind) <- List(Node(1), Node(2)).zipWithIndex) {
//        en.member(revEl) should be (true)
//      }
//    }
//
//    val normalTrees = constructEnumeratorNormal
//    
//    forAll(Gen.choose(1, 5), Gen.choose(1, 5), minSuccessful(20)) {
//      (size: Int, m: Int) =>
//      {
//        val normalList = normalTrees.getEnum( size, 1 to m )
//        
//        for ( ind <- 0 until normalList.size) {
//          trees( size, 1 to m ).member( normalList(ind) ) should be (true)
//        }
//        
//      }
//    }
//  }

  def constructEnumerator = {

    val colorsProducer = new WrapFunctionFin(
      (set: Set[Boolean]) => { new WrapArray(set.toArray) })

    val treesOfSize = new WrapFunctionFin(
      (self: MemberDependFinite[(Int, Range, Set[Boolean], Int), Tree], pair: (Int, Range, Set[Boolean], Int)) => {
        val (size, range, colors, blackHeight) = pair

        if (range.size >= size && range.size < 0 || blackHeight < 0) new Empty: MemberFinite[Tree]
        else
        if (size == 0 && blackHeight == 1 && colors.contains(true)) new Singleton(Leaf): MemberFinite[Tree]
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
            (p: (((Int, Int), Boolean), (Tree, Tree)) ) => {
              val (((leftSize, currRoot), rootColor), (leftTree, rightTree)) = p

              assert( ! (size >= 2 && leftSize == 0 && size - leftSize - 1 == 0) )
              assert( ! (size >= 2 && leftTree == Leaf && rightTree == Leaf ) )
              assert( !(leftSize > 0 && leftTree == Leaf), "leftSize=%d, leftTree=Leaf".format(leftSize))
              Node(leftTree, currRoot, rightTree, rootColor)
            }
            
          val invertTree = {
            ( p: Tree ) => {              
              val Node(leftTree, currRoot, rightTree, rootColor) = p.asInstanceOf[Node]

              (((RedBlackTrees.size(leftTree), currRoot), rootColor), (leftTree, rightTree))              
            }
          }

          new Map[(((Int, Int), Boolean), (Tree, Tree)), Tree]( allNodes, makeTree, invertTree )
            with MemberFinite[Tree]
            : MemberFinite[Tree]
        } else new Empty: MemberFinite[Tree]
      })

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
        else
        if (size == 0 && blackHeight == 1 && colors.contains(true)) e.Singleton(Leaf)
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

                assert( ! (size >= 2 && leftSize == 0 && size - leftSize - 1 == 0) )
                assert( ! (size >= 2 && leftTree == Leaf && rightTree == Leaf ) )
                assert( !(leftSize > 0 && leftTree == Leaf), "leftSize=%d, leftTree=Leaf".format(leftSize))
                Node(leftTree, currRoot, rightTree, rootColor)
              })

          allNodes
        } else e.Empty
      })

    treesOfSize
  }

}
