//package insynth
//package streams
//package benchmarks
//
//import org.scalatest._
//import org.scalameter.api._
//
//import dependent._
//import streams.{ light => e }
//
//import util._
//import util.logging._
//import Structures.BSTrees._
//
//class HeapArrayBenchmark extends DependentMemoizedBenchmark[Int, Dependent[(Int, Set[Int]), Tree]]
//  with java.io.Serializable with HasLogger {
//  import common._
//  import e.Enum
//
//  val maxSize = 10
//
//  fixture
//
//  type EnumType = Dependent[(Int, Set[Int]), Tree]
//
//  override def name = "Heap"
//
//  def measureCode(using: super.Using[Int], tdEnum: EnumType) = {
//    using in { (size: Int) =>
//      val enum = tdEnum.getStream((size, size))
//      val elements =
//        for ( ind <- 0 until enum.size ) yield enum(ind)
//    }
//  }
//
//  def generator = Gen.range("size")(1, maxSize, 1)
//
//  def warmUp(inEnum: EnumType) {
//    val tdEnum = inEnum.asInstanceOf[EnumType]
//    for (size <- 1 to maxSize) {
//      val enum= tdEnum.getStream((size, size))
//      val elements =
//        for (
//          ind <- 0 until enum.size
//        ) yield enum(ind)
//    }
//  }
//
//  def constructEnumerator(ms: MemoizationScope) = {
//    val rootProducer = Producer[Range, Int](
//      (range: Range) => {
//        e.WrapperArray(range)
//      })
//
//    val sizeProducer = Producer[Int, Int](
//      (size: Int) => {
//        e.WrapperArray(0 until size)
//      })
//      
//    val setChooser = Producer.memoized(
//      (self: Dependent[(Int, Set[Int]), Set[Int]], pair: (Int, Set[Int])) => {
//        val (size, set) = pair
//
//        if (size <= 0) e.Empty: e.Enum[Set[Int]]
//        else if (size == 1) e.Enum(set map { Set(_) }): e.Enum[Set[Int]]
//        else {
//          val roots = Enum(set)
//
//          val keptIn: Dependent[Int, Set[Int]] = new InMapper(self, { (chosen: Int) =>
//            (size - 1, set - chosen)
//          })
//
//          val leftOutIn: Dependent[Int, Set[Int]] = new InMapper(self, { (leftOut: Int) =>
//            (size, set - leftOut)
//          })
//          
//          val kept = BinaryFiniteMemoized.combine(roots, keptIn,
//            (root: Int, setOut: Set[Int]) => {
//              setOut + root
//            }
//          )
//          
//          val leftOut = BinaryFiniteMemoized.combine(roots, leftOutIn,
//            (root: Int, setOut: Set[Int]) => {
//              setOut
//            }
//          )
//          
//          val allNodes = e.RoundRobbin(kept, leftOut)
//          allNodes
//        }
//      })
//
//    Producer.memoized(
//      (self: EnumType, pair: (Int, Range)) => {
//        val (size, range) = pair
//
//        if (size <= 0) e.Singleton(Leaf)
//        else if (size == 1) e.WrapperArray(range map { v => Node(Leaf, v, Leaf) })
//        else {
//          val roots = rootProducer.getStream(range)
//          val leftSizes = sizeProducer.getStream(size)
//
//          val rootLeftSizePairs = e.Binary(leftSizes, roots)
//
//          val leftTrees: Dependent[(Int, Int), Tree] = new InMapper(self, { (par: (Int, Int)) =>
//            val (leftSize, median) = par
//            (leftSize, range.start to (median - 1))
//          })
//
//          val rightTrees: Dependent[(Int, Int), Tree] =
//            new InMapper(self, { (par: (Int, Int)) =>
//              val (leftSize, median) = par
//              (size - leftSize - 1, (median + 1) to range.end)
//            })
//
//          val leftRightPairs: Dependent[(Int, Int), (Tree, Tree)] =
//            CoupledBinary(leftTrees, rightTrees)
//
//          import BinaryFiniteMemoized._
//
//          val allNodes =
//            combine[(Int, Int), (Tree, Tree), Node](rootLeftSizePairs, leftRightPairs,
//              (p1: (Int, Int), p2: (Tree, Tree)) => {
//                val ((leftSize, currRoot), (leftTree, rightTree)) = (p1, p2)
//
//                Node(leftTree, currRoot, rightTree)
//              })
//
//          allNodes
//        }
//      })
//  }
//
//}
