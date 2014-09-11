//package insynth
//package enumeration
//package insynth.enumeration.benchmarks.test
//
//import dependent._
//import insynth.{ enumeration => e }
//import memoization._
//
//import insynth.util._
//import insynth.util.logging._
//import structures.BSTrees._
//
//import org.scalatest._
//import org.scalameter.api._
//
//import scala.language.postfixOps
//import scala.language.existentials
//
//class HeapArrayBenchmark
//  extends structuresBenchmark[Depend[(Int, Array[Int]), Tree]]
////  extends DependentMemoizedBenchmark[Int, Depend[(Int, List[Int]), Tree]]
//  with java.io.Serializable with HasLogger {
//  //  import e.Enum
//
//  override def name = "HeapArray"
//
//  val maxSize = BenchmarkSuite.maxSize
//
//  fixture
//
//  type EnumType = Depend[(Int, Array[Int]), Tree]
//
//  def measureCode(using: super.Using[Int], tdEnum: EnumType) = {
//    using in { (size: Int) =>
//      val enum = tdEnum.getEnum((size, rangeList(size)))
//      val elements =
//        for ( ind <- 0 until enum.size ) yield enum(ind)
//    }
//  }
//
////  def generator = Gen.range("size")(1, maxSize, 1)
//
//  def warmUp(inEnum: EnumType) {
//    val tdEnum = inEnum.asInstanceOf[EnumType]
//    for (size <- 1 to maxSize) {
//      val enum= tdEnum.getEnum((size, rangeList(size)))
//      val elements =
//        for (
//          ind <- 0 until enum.size
//        ) yield enum(ind)
//    }
//  }
//  
//  def rangeList(m: Int) = m to 0 by -1 toArray
//
//  def constructEnumerator(implicit ms: MemoizationScope) = {
//    Depend.memoized(
//      (self: EnumType, pair: (Int, Array[Int])) => {
//      // list sorted descendingly
//      val (size, array) = pair
//
//      if (size <= 0) e.Singleton(Leaf)
//      else if (size == 1)
////        (e.Enum(list): Enum[Int]) map { v => Node(Leaf, v, Leaf) }
//        e.WrapArray( array map { v => Node(Leaf, v, Leaf) } )
//      else if (!array.isEmpty) {
//        val rootsInds = Enum(0 until array.size)
//
//        val childHeaps = InMap(self, { (rootInd: Int) =>
//          ( (size-1)/2, array.drop(rootInd) )
//        })
//        val leftRightPairs: Depend[Int, (Tree, Tree)] =
//          Product(childHeaps, childHeaps)
//        
//        val allNodes =
//          memoization.Chain[Int, (Tree, Tree), Node](rootsInds, leftRightPairs,
//            (rootInd: Int, p2: (Tree, Tree)) => {
//              val (leftTree, rightTree) = p2
//
//              Node(leftTree, array(rootInd), rightTree)
//            })
//
//        allNodes
//      } else e.Empty
//    })
//  }
//
//}
