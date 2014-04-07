//package insynth
//package streams
//package benchmarks
//
//import org.scalameter.api._
//import util.logging._
//
//import dependent._
//import streams.{ light => e }
//import util.Structures.RedBlackTrees._
//
//class RedBlackTreeDependentBenchmark extends DependentMemoizedBenchmark[Int, Dependent[(Int, Range, Set[Boolean], Int), Tree]]
//  with java.io.Serializable with HasLogger {
//
//  val maxSize = 2
//
//  fixture
//
//  type EnumType = Dependent[(Int, Range, Set[Boolean], Int), Tree]
//
//  override val name = "Red Black Tree"
//
//  def measureCode(using: super.Using[Int], tdEnum: EnumType) = {
//    using in { (size: Int) =>
//      val elements =
//        for (
//          blackHeight <- 0 to (size + 1);
//          enum = tdEnum.getStream(size, 1 to size, Set(true, false), blackHeight);
//          ind <- 0 until enum.size
//        ) yield enum(ind)
//    }
//  }
//
//  def generator = Gen.range("size")(1, maxSize, 1)
//
//  def warmUp(inEnum: EnumType) {
//    val tdEnum = inEnum.asInstanceOf[Dependent[(Int, Range, Set[Boolean], Int), Tree]]
//    for (size <- 1 to maxSize) {
//      val tdEnumVal = tdEnum
//      val elements =
//        for (
//          blackHeight <- 0 to (size + 1);
//          enum = tdEnum.getStream(size, 1 to size, Set(true, false), blackHeight);
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
//    val colorsProducer = Producer[Set[Boolean], Boolean](
//      (set: Set[Boolean]) => {
//        e.WrapperArray(set.toIndexedSeq)
//      })
//
//    val sizeProducer = Producer[Int, Int](
//      (size: Int) => {
//        e.WrapperArray(0 until size)
//      })
//
//    val treesOfSize: Dependent[(Int, Range, Set[Boolean], Int), Tree] = Producer.memoized(
//      (self: Dependent[(Int, Range, Set[Boolean], Int), Tree], pair: (Int, Range, Set[Boolean], Int)) => {
//        val (size, range, colors, blackHeight) = pair
//
//        if (size == 0 || range.size < 0 || colors.isEmpty || blackHeight < 0) e.Empty
//        if (size == 0 && blackHeight == 1 && colors.contains(true)) e.Singleton(Leaf)
//        else if (size == 1) e.WrapperArray(range map { v => Node(Leaf, v, Leaf, true) })
//        else if (size > 0 && blackHeight >= 1) {
//          val roots = rootProducer.getStream(range)
//          val leftSizes = sizeProducer.getStream(size)
//          val rootColors = colorsProducer.getStream(colors)
//
//          val rootLeftSizePairs = e.Binary(leftSizes, roots)
//          val rootLeftSizeColorTuples = e.Binary(rootLeftSizePairs, rootColors)
//
//          val leftTrees: Dependent[((Int, Int), Boolean), Tree] = new InMapper(self, { (par: ((Int, Int), Boolean)) =>
//            val ((leftSize, median), rootColor) = par
//            val childColors = if (rootColor) Set(true, false) else Set(true)
//            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
//            (leftSize, range.start to (median - 1), childColors, childBlackHeight)
//          })
//
//          val rightTrees: Dependent[((Int, Int), Boolean), Tree] = new InMapper(self, { (par: ((Int, Int), Boolean)) =>
//            val ((leftSize, median), rootColor) = par
//            val childColors = if (rootColor) Set(true, false) else Set(true)
//            val childBlackHeight = if (rootColor) blackHeight - 1 else blackHeight
//            (size - leftSize - 1, (median + 1) to range.end, childColors, childBlackHeight)
//          })
//
//          val leftRightPairs: Dependent[((Int, Int), Boolean), (Tree, Tree)] =
//            CoupledBinary(leftTrees, rightTrees)
//
//          import BinaryFiniteMemoized._
//
//          val allNodes =
//            combine[((Int, Int), Boolean), (Tree, Tree), Node](rootLeftSizeColorTuples, leftRightPairs,
//              (p1: ((Int, Int), Boolean), p2: (Tree, Tree)) => {
//                val (((leftSize, currRoot), rootColor), (leftTree, rightTree)) = (p1, p2)
//
//                Node(leftTree, currRoot, rightTree, rootColor)
//              })
//
//          allNodes
//        } else e.Empty
//      })
//
//    treesOfSize
//  }
//
//}