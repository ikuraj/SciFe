//package scife
//package enumeration
//package iterable
//package lzy
//package testcase
//
//import scife.{ enumeration => e }
//import e.iterable._
//import e.dependent._
//
//import scife.util.logging._
//import scife.util._
//
//import org.scalatest._
//import org.scalatest.prop._
//import org.scalacheck.Gen
//
//import scala.language.postfixOps
//
//import org.scalatest.junit.JUnitRunner
//import org.junit.runner.RunWith
//
//@RunWith(classOf[JUnitRunner])
//class SortedListTest extends FunSuite with Matchers
//  with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {
//  
//  import common._
//  import enumdef._
//  import scife.util.structures._
//  
//  type EnumType[A] = LazyEnumFinite[A]/* with Memoized[A]*/
//  type DepEnumType[I, O] = Depend[I, O] { type EnumSort[A] = SortedListTest.this.EnumType[A] }
//  type DepEnumTypeFinite[I, O] = DependFinite[I, O] { type EnumSort[A] = SortedListTest.this.EnumType[A] }
//  
//  type Collection[A] = Stream[A]
// 
//  implicit val ms = e.memoization.scope.NoScope 
//
//  ignore("correctness of enumeration") {
//    val enum = constructEnumerator
//    val base = SortedListEnum.constructEnumeratorStrict
//
////    forAll( Gen.choose(0, 10), Gen.choose(0, 10), minSuccessful(10) ) {
////      case (size, range) =>
////        whenever (range > size) {
//    for (size <- 0 to 7; range <- 0 to 7; if range > size) {
//      enum(size, range).size shouldBe base(size, range).size
//      for(i <- 0 until enum.size)
//        enum(size, range)(i) shouldBe base(size, range)(i)
//    }
//
//  }
//  
//  test("finding an element in a list") {
//    val listEnum = constructEnumerator
//    val testTuples =
//      new iterable.ProductFinite[Int, Collection[Int]](
//        new e.WrapArray( Array(3, 3, 3) ) with ResetIter[Int],
//        listEnum(3, 3)
//      )
//      
//    testTuples.size shouldBe 30
//      
//    for (i <- 1 to 3) {
//      testTuples.hasNext shouldBe true
//      val p = testTuples.next
//      p._2.mkString("")
//    }
//    testTuples.reset
//
//    for (i <- 1 to testTuples.size) {
//      testTuples.hasNext shouldBe true
//      val p = testTuples.next
//      p._2.mkString("")
//    }
//    testTuples.hasNext shouldBe false
//    testTuples.reset
//    
//    var count = 0
//    while (testTuples.hasNext) {
//      val (n, l) = testTuples.next
//      count += 1
//      val index = l.indexOf(n)
//    }
//    count shouldBe testTuples.size + (- 6 + 1) * 3 
//
//  }
//  
//  test("correctness of iterative enumeration") {
//    val enum = constructEnumeratorStrict
//    
//    {
//      val itEnum = enum(3, 3).asInstanceOf[
//        iterable.lzy.dependent.ChainFiniteSingleCombine[Int, List[Int], List[Int]]]
//      itEnum.rightStreams.size shouldBe 1
//      itEnum.size shouldBe 1
//      
//      itEnum.categoryIndex shouldBe 0
//      itEnum.hasNext shouldBe true
//      itEnum.categoryIndex shouldBe 0
//      
//      itEnum.next
//      itEnum.categoryIndex shouldBe 0
//      itEnum.rightStreams(0).touched shouldBe false
//
//      itEnum.hasNext shouldBe false
//    }
//
//    for (size <- 2 to 6; range <- 2 to 6; if range > size) {
////    { val size = 2; val range = 4;
//      withClue(s"size=$size, range=$range") {
//        val itEnum = enum(size, range).asInstanceOf[
//          iterable.lzy.dependent.ChainFiniteSingleCombine[Int, Collection[Int], Collection[Int]]]
//        for(i <- 1 to itEnum.rightStreams.size) {
//          itEnum.hasNext shouldBe true
//          itEnum.next
//        }
//        
//        itEnum.hasNext shouldBe false
//      }
//    }
//
//  }
//
//  test("correctness of exhaustive iterative enumeration") {
//    val enum = constructEnumerator
//    val base = SortedListEnum.constructEnumerator
//
//    for (size <- 0 to 7; range <- 0 to 7; if range > size) {
//      val itEnum = enum(size, range)
//      val baseEnum = base(size, range)
//
//      itEnum.size shouldBe baseEnum.size
//
//      for(i <- 0 until baseEnum.size)
//        itEnum.next shouldBe baseEnum(i)
//      
//      itEnum.hasNext shouldBe false
//    }
//
//  }
//  
//  test("correctness of exhaustive iterative enumeration, next") {
//    val enum = constructEnumerator
//
//    for (size <- 0 to 7; range <- 0 to 7; if range > size) {
//      val itEnum = enum(size, range)
//      while (itEnum.hasNext) itEnum.next
//    }
//
//  }
//  
//  test("correctness of exhaustive iterative enumeration, next, reset") {
//    val enum = constructEnumerator
//    val base = SortedListEnum.constructEnumerator
//
//    for (size <- 0 to 7; range <- 0 to 7; if range > size) {
//      val itEnum = enum(size, range)
//      val baseEnum = base(size, range)
//
//      for (_ <- 1 to 3) {
//        itEnum.size shouldBe baseEnum.size
//  
//        for(i <- 0 until baseEnum.size) {
//          itEnum.hasNext shouldBe true
//          itEnum.next shouldBe baseEnum(i)
//        }
//        
//        itEnum.hasNext shouldBe false
//        itEnum.reset
//      }
//    }
//
//  }
//
//  ignore("resetting enumerator in the middle") {
//    val depEnum = constructEnumerator
//    val enum = depEnum(7, 7)
//
//    val halfOfTheEnum =
//      for (ind <- 0 until enum.size/2)
//        yield enum(ind)
//        
//    enum.reset
//    
//    for ((el, ind) <- halfOfTheEnum.zipWithIndex)
//      enum(ind) should be (el)
//
//  } 
//  
//  def constructEnumerator(implicit ms: e.memoization.MemoizationScope) = {
//
//    new WrapFunctionTest[(Int, Int), Collection[Int], EnumType](
//      ( self: DepEnumType[(Int, Int), Collection[Int]], pair: (Int, Int) ) => {
//        val (size, max) = pair
//
//        if (size == 0) (new e.Singleton( Stream[Int]() ) with ResetIter[Collection[Int]] with
//          Touchable[Collection[Int]]): EnumType[Collection[Int]]
//        else //if (size > 0)
//          {
//            val roots = new e.WrapArray( 1 to max toArray ) with ResetIter[Int] with Touchable[Int]
//  
//            val innerCollections: DepEnumType[Int, Collection[Int]] = new InMap[(Int, Int), Int, Collection[Int],
//              DepEnumType] (self, { (par: Int) =>
//              (size - 1, par)
//            }) with DependFinite[Int, Collection[Int]] {
//              override type EnumSort[A] = SortedListTest.this.EnumType[A]
//            }
//  
//            val fConstructCollection: (=> Int, => Collection[Int]) => Collection[Int] =
//              (head, l) => {
//                info("fConstructCollection invoked!!")
//                head #:: l
//              }
//            val allCollections =
//              new iterable.lzy.dependent.ChainFiniteSingleCombine[Int, Collection[Int], Collection[Int]](
//                roots,
//                innerCollections.asInstanceOf[DepEnumTypeFinite[Int, Collection[Int]]],
//                fConstructCollection
//              ) with ResetIter[Collection[Int]] with Touchable[Collection[Int]]
//  
//            allCollections: LazyEnumFinite[Collection[Int]]
//        }
//      }
//    )
//  }
//  
//  def constructEnumeratorStrict(implicit ms: e.memoization.MemoizationScope) = {
//
//    new WrapFunctionTest[(Int, Int), Collection[Int], EnumType](
//      ( self: DepEnumType[(Int, Int), Collection[Int]], pair: (Int, Int) ) => {
//        val (size, max) = pair
//
//        if (size == 0) (new e.Singleton( Stream[Int]() ) with ResetIter[Collection[Int]] with
//          Touchable[Collection[Int]]): EnumType[Collection[Int]]
//        else //if (size > 0)
//          {
//            val roots = new e.WrapArray( 1 to max toArray ) with ResetIter[Int] with Touchable[Int]
//  
//            val innerCollections: DepEnumType[Int, Collection[Int]] = new InMap[(Int, Int), Int, Collection[Int],
//              DepEnumType] (self, { (par: Int) =>
//              (size - 1, par - 1)
//            }) with DependFinite[Int, Collection[Int]] {
//              override type EnumSort[A] = SortedListTest.this.EnumType[A]
//            }
//  
//            val fConstructCollection: (=> Int, => Collection[Int]) => Collection[Int] =
//              (head, l) => {
//                info("fConstructCollection invoked!!")
//                head #:: l
//              }
//            val allCollections =
//              new iterable.lzy.dependent.ChainFiniteSingleCombine[Int, Collection[Int], Collection[Int]](
//                roots,
//                innerCollections.asInstanceOf[DepEnumTypeFinite[Int, Collection[Int]]],
//                fConstructCollection
//              ) with ResetIter[Collection[Int]] with Touchable[Collection[Int]]
//  
//            allCollections: LazyEnumFinite[Collection[Int]]
//        }
//      }
//    )
//  }
//
//}
