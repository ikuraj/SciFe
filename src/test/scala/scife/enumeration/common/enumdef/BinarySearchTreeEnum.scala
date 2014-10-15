package scife
package enumeration
package common.enumdef

import scife.{ enumeration => e }
import scife.util._

import scala.language.postfixOps

object BinarySearchTreeEnum {
  
  import structures.BSTrees._
  import dependent._
  import memoization._
  
  type EnumType = Depend[(Int, Range), Tree]
 
  val enumDefList =
    List(
      constructEnumeratorBenchmark(_: MemoizationScope),
      constructEnumTestcase(_: MemoizationScope),
      constructBenchmarkOld(_: MemoizationScope),
      constructEnumeratorBenchmarkNoTuplesWhenConstructingTree(_: MemoizationScope)
    ) zip List(
      "constructEnumeratorBenchmark", "constructEnumTestcase", "constructBenchmarkOld",
      "constructEnumeratorBenchmarkNoTuplesWhenConstructingTree" 
    )
  
  // slightly changed constructEnumeratorBenchmark
  def constructEnumeratorBenchmarkNoTuplesWhenConstructingTree(implicit ms: MemoizationScope) = {
    Depend.memoized(
      (self: Depend[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        if (size <= 0) e.Singleton(Leaf)
        else if (size == 1)
          e.WrapArray(range map { v => Node(Leaf, v, Leaf) })
        else {
          val roots = e.Enum(range)
          val leftSizes = e.Enum(0 until size)

          val rootLeftSizePairs = e.Product(leftSizes, roots)

          val leftTrees: Depend[(Int, Int), Tree] = InMap(self, { (par: (Int, Int)) =>
            val (leftSize, median) = par
            (leftSize, range.start to (median - 1))
          })

          val rightTrees: Depend[(Int, Int), Tree] =
            InMap(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (size - leftSize - 1, (median + 1) to range.end)
            })

          val leftRightPairs: Depend[(Int, Int), (Tree, Tree)] =
            Product(leftTrees, rightTrees)

          val allNodes =
            memoization.Chain[(Int, Int), (Tree, Tree), Node](rootLeftSizePairs, leftRightPairs,
              (p1: (Int, Int), p2: (Tree, Tree)) => Node(p2._1, p1._2, p2._2)
            )

          allNodes
        }
      })
  }
  
  def constructEnumeratorBenchmark(implicit ms: MemoizationScope) = {
    Depend.memoized(
      (self: Depend[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        if (size <= 0) e.Singleton(Leaf)
        else if (size == 1)
          e.WrapArray(range map { v => Node(Leaf, v, Leaf) })
        else {
          val roots = e.Enum(range)
          val leftSizes = e.Enum(0 until size)

          val rootLeftSizePairs = e.Product(leftSizes, roots)

          val leftTrees: Depend[(Int, Int), Tree] = InMap(self, { (par: (Int, Int)) =>
            val (leftSize, median) = par
            (leftSize, range.start to (median - 1))
          })

          val rightTrees: Depend[(Int, Int), Tree] =
            InMap(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (size - leftSize - 1, (median + 1) to range.end)
            })

          val leftRightPairs: Depend[(Int, Int), (Tree, Tree)] =
            Product(leftTrees, rightTrees)

          val allNodes =
            memoization.Chain[(Int, Int), (Tree, Tree), Node](rootLeftSizePairs, leftRightPairs,
              (p1: (Int, Int), p2: (Tree, Tree)) => {
                val ((leftSize, currRoot), (leftTree, rightTree)) = (p1, p2)

                Node(leftTree, currRoot, rightTree)
              })

          allNodes
        }
      })
  }
  
  // has asserts
  def constructEnumTestcase(implicit ms: MemoizationScope) = {
    
    val rootProducer: Depend[Range, Int] = Depend(
      (range: Range) => {
        e.WrapArray( range )
      }
    )

    val sizeProducer = Depend(
      (size: Int) => {
        e.WrapArray( 0 until size )
      }
    )

    var getTreeOfSize: Depend[ (Int, Range), Tree ] = null

    val treesOfSize: Depend[ (Int, Range), Tree ] = Depend.memoized(
      ( pair: (Int, Range) ) => {
        val (size, range) = pair
        assert(size >= 0, "size=%d, range=%s" format (size, range))

        // do not care about the range, size is important (rangeProduced can return Empty)
        if (size <= 0) e.Singleton( Leaf )
        else if (size == 1) e.WrapArray( range map { v => Node(Leaf, v, Leaf) } )
        else {
          val roots = rootProducer.getEnum(range)
          val leftSizes = sizeProducer.getEnum(size)

          val rootLeftSizePairs = e.Product(leftSizes, roots)

//          val forBothTreesPairs = e.Mapper(rootLeftSizePairs, { (p: (Int, Int)) =>
//            val (root, leftSize) = p
//            (root,
//              leftSize - 1, range.start to (root - 1),
//              size - leftSize - 1, (root + 1) to range.end
//            )
//          })

          val leftTrees: Depend[(Int, Int), Tree] = InMap(getTreeOfSize, { (par: (Int, Int)) =>
            val (leftSize, median) = par
            (leftSize, range.start to (median - 1))
          })

          val rightTrees: Depend[(Int, Int), Tree] =
            InMap(getTreeOfSize, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (size - leftSize - 1, (median + 1) to range.end)
            })

          val leftRightPairs: Depend[(Int, Int), (Tree, Tree)] =
            Product(leftTrees, rightTrees)

          val allNodes =
            memoization.Chain[(Int, Int), (Tree, Tree), Node](rootLeftSizePairs, leftRightPairs,
//              (leftSize: Int, mid: Int) => (size - 1, range.start to (mid - 1)),
              (p1: (Int, Int), p2: (Tree, Tree)) => {
                val ((leftSize, currRoot), (leftTree, rightTree)) = (p1, p2)

                assert( ! (size >= 2 && leftSize == 0 && size - leftSize - 1 == 0) )
                assert( ! (size >= 2 && leftTree == Leaf && rightTree == Leaf ) )
                assert( !(leftSize > 0 && leftTree == Leaf), "leftSize=%d, leftTree=Leaf".format(leftSize))
                Node( leftTree, currRoot, rightTree )
              }
            )

          allNodes
        }
      }
    )

    getTreeOfSize = treesOfSize
    
    treesOfSize

  }
  
  // has tests
  def constructBenchmarkOld(implicit ms: MemoizationScope) = {
    
    import org.scalatest.Matchers._
    
    val rootProducer = Depend.memoized(
      (range: Range) => {
        e.WrapArray(range)
      })

    val sizeProducer = Depend.memoized(
      (size: Int) => {
        e.WrapArray(0 until size)
      })

    Depend.memoizedFin(
      (self: DependFinite[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        if (size <= 0) e.Singleton(Leaf)
        else if (size == 1) e.WrapArray(range map { v => Node(Leaf, v, Leaf) })
        else {
          val roots = rootProducer.getEnum(range)
          val leftSizes = sizeProducer.getEnum(size)

          val rootLeftSizePairs = e.Product(leftSizes, roots)

          val leftTrees: DependFinite[(Int, Int), Tree] =
            InMap(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (leftSize, range.start to (median - 1))
            })

          val rightTrees: DependFinite[(Int, Int), Tree] =
            InMap(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (size - leftSize - 1, (median + 1) to range.end)
            })

          val leftRightPairs: Depend[(Int, Int), (Tree, Tree)] =
            Product(leftTrees, rightTrees)

          val allNodes =
            memoization.Chain[(Int, Int), (Tree, Tree), Node](rootLeftSizePairs, leftRightPairs,
              (p1: (Int, Int), p2: (Tree, Tree)) => {
                val ((leftSize, currRoot), (leftTree, rightTree)) = (p1, p2)

                Node(leftTree, currRoot, rightTree)
              })(ms)

          allNodes shouldBe a [Memoized[_]]

          allNodes: Finite[Node]
        }
      })
  }

}
