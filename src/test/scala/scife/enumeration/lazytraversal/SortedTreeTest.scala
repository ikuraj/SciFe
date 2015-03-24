package scife
package enumeration
package lazytraversal

import scife.{ enumeration => e }
import e.iterable._
import e.dependent._

import scalaz.LazyTuple2

import scife.util.logging._
import scife.util._

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen

import scala.language.postfixOps

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class SortedTreeTest extends FunSuite with Matchers
  with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {

  import common._
  import enumdef._
  import scife.util.structures._
  import LazyBSTrees._

  type EnumType[A] = Finite[A] with Touchable[A] with Resetable[A] with Skippable[A]
  type DepEnumType[I, O] = Depend[I, O] { type EnumSort[A] = SortedTreeTest.this.EnumType[A] }
  type DepEnumTypeFinite[I, O] = DependFinite[I, O] { type EnumSort[A] = SortedTreeTest.this.EnumType[A] }

  implicit val ms = e.memoization.scope.NoScope

  import e.common.enumdef.BinarySearchTreeEnum.{ constructEnumeratorBenchmark => constructStrict }

  implicit val lazy2reg = LazyBSTrees.toRegularBSTTree _

  implicit val treeTag = implicitly[reflect.ClassTag[scife.util.structures.LazyBSTrees.Tree]]
  implicit val pairTag = implicitly[reflect.ClassTag[(Int, Int)]]

  // TODO move to separate tests
  //      test("product") {
  //        val e1 = new e.WrapArray(Array(1, 2, 3)) with Touchable[Int] with Resetable[Int]
  //        val e2 = new e.WrapArray(Array(1, 2, 3)) with Touchable[Int] with Resetable[Int]
  //    
  //        val lzy = Product.touchable(e1, e2)
  //    
  //        for (i <- 0 to 5) {
  //          val tuple = lzy(i)
  //          tuple._1
  //          tuple._2
  //    
  //          val nextInd = lzy.next(i)
  //          nextInd shouldBe (i + 1)
  //    
  //        }
  //    
  //      }
  //    
  //      test("product, right touched") {
  //        val e1 = new e.WrapArray(Array(1, 2, 3, 4)) with Touchable[Int] with Resetable[Int]
  //        val e2 = new e.WrapArray(Array(1, 2, 3, 4)) with Touchable[Int] with Resetable[Int]
  //    
  //        val lzy = Product.touchable(e1, e2)
  //    
  //        var nextInd = 0
  //        for (i <- 0 to 5) {
  //          lzy.reset
  //          val tuple = lzy(i)
  //          tuple._1
  //          tuple._2
  //    
  //          nextInd = lzy.next(i)
  //          nextInd shouldBe (i + 1)
  //        }
  //    
  //        nextInd shouldBe 6
  //        var inds = List.empty[Int]
  //        //    lzy(nextInd)
  //        while (nextInd < lzy.size) {
  //          lzy.reset
  //          val tuple = lzy(nextInd)
  //          tuple._2
  //    
  //          nextInd = lzy.next(nextInd)
  //          inds = nextInd :: inds
  //        }
  //    
  //        inds shouldBe List(16, 12, 8)
  //    
  //      }
  //    
  //      test("product, left touched") {
  //        val e1 = new e.WrapArray(Array(1, 2, 3, 4)) with Touchable[Int] with Resetable[Int]
  //        val e2 = new e.WrapArray(Array(1, 2, 3, 4)) with Touchable[Int] with Resetable[Int]
  //    
  //        val lzy = Product.touchable(e1, e2)
  //    
  //        var nextInd = 0
  //        for (i <- 0 to 5) {
  //          lzy.reset
  //          val tuple = lzy(i)
  //          tuple._1
  //          tuple._2
  //    
  //          nextInd = lzy.next(i)
  //          nextInd shouldBe (i + 1)
  //        }
  //    
  //        var inds = List.empty[Int]
  //        nextInd shouldBe 6
  //        while (nextInd < lzy.size) {
  //          lzy.reset
  //          val tuple = lzy(nextInd)
  //          tuple._1
  //    
  //          nextInd = lzy.next(nextInd)
  //          inds = nextInd :: inds
  //        }
  //    
  //        inds shouldBe List(16, 9, 8, 7)
  //    
  //      }
  //    
  //      test("product, no touched") {
  //        val e1 = new e.WrapArray(Array(1, 2, 3, 4)) with Touchable[Int] with Resetable[Int]
  //        val e2 = new e.WrapArray(Array(1, 2, 3, 4)) with Touchable[Int] with Resetable[Int]
  //    
  //        val lzy = Product.touchable(e1, e2)
  //    
  //        var nextInd = 0
  //        for (i <- 0 to 4) {
  //          lzy.reset
  //          val tuple = lzy(i)
  //          tuple._1
  //          tuple._2
  //    
  //          nextInd = lzy.next(i)
  //          nextInd shouldBe (i + 1)
  //        }
  //    
  //        var inds = List.empty[Int]
  //        nextInd shouldBe 5
  //        while (nextInd < lzy.size) {
  //          lzy.reset
  //          val tuple = lzy(nextInd)
  //    
  //          nextInd = lzy.next(nextInd)
  //          inds = nextInd :: inds
  //        }
  //    
  //        inds shouldBe List(16)
  //    
  //      }
  //    
  //      test("product, mixed touched") {
  //        val e1 = new e.WrapArray(Array(1, 2, 3, 4)) with Touchable[Int] with Resetable[Int]
  //        val e2 = new e.WrapArray(Array(1, 2, 3, 4)) with Touchable[Int] with Resetable[Int]
  //    
  //        val lzy = Product.touchable(e1, e2)
  //    
  //        var nextInd = 0
  //          lzy.reset
  //        var t = lzy(nextInd)
  //        t._1
  //        t._2
  //        lzy.next(0) shouldBe 1
  //    
  //          lzy.reset
  //        t = lzy(1)
  //        t._2
  //        lzy.next(1) shouldBe 4
  //    
  //          lzy.reset
  //        t = lzy(4)
  //        t._1
  //        t._2
  //        lzy.next(4) shouldBe 5
  //    
  //          lzy.reset
  //        t = lzy(5)
  //        t._1
  //        lzy.next(5) shouldBe 6
  //    
  //          lzy.reset
  //        t = lzy(6)
  //        t._1
  //        t._2
  //        lzy.next(6) shouldBe 7
  //    
  //          lzy.reset
  //        t = lzy(7)
  //        t._1
  //        lzy.next(7) shouldBe 8
  //    
  //      }
  //  //  //
  //  //  //  //  ignore("correctness of enumeration") {
  //  //  //  //    val enum = constructEnumerator
  //  //  //  //    val base = constructStrict
  //  //  //  //
  //  //  //  //    for (size <- 0 to 5) {
  //  //  //  //      val range = 1 to size
  //  //  //  //      enum((size, range)).size shouldBe base(size, range).size
  //  //  //  //      for (i <- 0 until enum.size)
  //  //  //  //        (enum((size, range)).next: BSTrees.Tree) shouldBe base(size, range)(i)
  //  //  //  //    }
  //  //  //  //
  //  //  //  //  }
  //  //  //  //
  //  test("insert testing") {
  //
  //    for ((size, givenCnt) <- (1 to 6 zip 
  ////      List(1, 4, 12 ,35 ,111 ,376 ,1334 ,4866)
  //      List(1, 4, 11 , 26 , 63, 162)
  //    )) {
  //      val dEnum = constructEnumerator
  //      var count = 0
  //      val enum = dEnum(size, 1 to size)
  //      for (i <- 1 to size) {
  //        var nextInd = 0
  //        while (nextInd < enum.size) {
  //          enum.reset
  //          val t = enum(nextInd)
  //          val index = LazyBSTrees.insert(t, i)
  ////          toRegularBSTTree(t)
  //          nextInd = enum.next(nextInd)
  //          count += 1
  //        }
  //      }
  //      count shouldBe givenCnt
  ////      println(s"For size $size, count is $count / ${enum.size * size}")
  //    }
  //
  //  }
  
  test("numbers") {
    {
     val size = 14 
    val dEnum: DepEnumType[((Int, Range), Ugly), Tree] = consMess(ms, treeTag)
    //    
    //        testTuples.size shouldBe 15

    val n: Ugly = null
        var count = 0
        var enum = dEnum((size, 1 to size), null)
        for (i <- 1 to size) {
          enum.hardReset
//          enum = dEnum((size, 1 to size), null)
          var nextInd = 0
          while (nextInd < enum.size) {
            enum.reset
            val t = enum(nextInd)
//            val index = LazyBSTrees.insert(t, i)
//            val index = t insert i
//            index.lazyInvariant
  //          toRegularBSTTree(t)
            nextInd = enum.next(nextInd)
            count += 1
          }
        }
//        count shouldBe givenCnt
        println(s"For size $size, count is $count / ${enum.size * size}")
      }
  }
  
  //
  //  test("insert into a tree") {
  //    implicit def parTrans(p: (Int, Range)) =
  //      (p._1, p._2, null)
  //    
  //    val dEnum = constructEnumerator
  //    val testTuples =
  //      lazytraversal.Product.touchable[Int, Tree](
  //        //          lazytraversal.Product.touchableStrictPair[Int, Tree](
  //        new e.WrapArray(Array(1)) with Touchable[Int] with Resetable[Int],
  //        dEnum(3, 1 to 3))
  //    //    
  //    //        testTuples.size shouldBe 15
  //
  //    var count = 0
  //    for (i <- 1 to 3) {
  //      var nextInd = 0
  //      val enum = dEnum(i, 1 to i)
  //      while (nextInd < enum.size) {
  //        enum.reset
  //        val t = enum(nextInd)
  //        nextInd = enum.next(nextInd)
  //        count += 1
  //      }
  //    }
  //    count shouldBe 6
  //
  //    {
  //      var count = 0
  //      for (i <- 1 to 3) {
  //        var nextInd = 0
  //        val enum = dEnum(i, 1 to i)
  //        while (nextInd < enum.size) {
  //          enum.reset
  //          val t = enum(nextInd)
  //          t match {
  //            case Leaf =>
  //            case n: Node =>
  //              n.v
  //          }
  //          nextInd = enum.next(nextInd)
  //          count += 1
  //        }
  //      }
  //      count shouldBe 6
  //    }
  //
  //    {
  //      for (i <- 1 to 3) {
  //        var nextInd = 0
  //        val enum = dEnum(i, 1 to i)
  //        enum.reset
  //        val t = enum(0)
  //        t match {
  //          case Leaf =>
  //          case n: Node =>
  //            val bv = n.v
  //            n.l
  //        }
  //      }
  //    }
  //    {
  //      var count = 0
  //      for (i <- 1 to 3) {
  //        var nextInd = 0
  //        val enum = dEnum(i, 1 to i)
  //        while (nextInd < enum.size) {
  //          enum.reset
  //          val t = enum(nextInd)
  //          t match {
  //            case Leaf =>
  //            case n: Node =>
  //              val a = n.v
  //              n.l
  //          }
  //          val newInd = enum.next(nextInd)
  //          newInd should be > (nextInd)
  //          nextInd = newInd
  //          count += 1
  //        }
  //      }
  //      count shouldBe 7
  //    }
  //    {
  //      var lzyAll = List[Tree]()
  //      var count = 0
  //      for (i <- 3 to 3) {
  //        var nextInd = 0
  //        val enum = dEnum(i, 1 to i)
  //        while (nextInd < enum.size) {
  //          enum.reset
  //          println("reset")
  //          val t = enum(nextInd)
  //          t match {
  //            case Leaf =>
  //            case n: Node =>
  //              n.l != n.r
  //              lzyAll = n :: lzyAll
  //          }
  //          val newInd = enum.next(nextInd)
  //          newInd should be > (nextInd)
  //          nextInd = newInd
  //          count += 1
  //        }
  //      }
  //      val all = testTuples.toList.map(_._2)
  //
  //      println("Touched\n" + (all.toSet.intersect(lzyAll.toSet).mkString("\n")))
  //      println(s"All:\n${all.toSet.mkString("\n")}")
  //      println(s"Missing:\n${all.toList.indexOf(all.toSet.diff(lzyAll.toSet).head)}")
  //
  //      count shouldBe 5
  //    }
  //    //
  //    //    for (i <- 1 to 8) {
  //    //      testTuples.hasNext shouldBe true
  //    //      val p = testTuples.next
  //    //    }
  //    //    testTuples.hasNext shouldBe false
  //    //    testTuples.reset
  //    //
  //    def tupleToString[A, B](t: LazyTuple2[A, B]) =
  //      s"[${t._1.toString}, ${t._2.toString}]"
  //
  ////    println(testTuples.toList.map(tupleToString).mkString("\n"))
  //    //          
  //    //          testTuples.reset
  //    //          val t = testTuples(0)
  //    //          val n = t._1
  //    //          val l = t._2
  //    //          testTuples.next(0) should be > (1)
  //
  //    //        var count = 0
  //    //        var nextInd = 0
  //    //        while (nextInd < testTuples.size) {
  //    ////          val (n, l) = testTuples(nextInd)
  //    //          testTuples.reset
  //    //          val t = testTuples(nextInd)
  //    //          val n = t._1
  //    //          val l = t._2
  //    //          count += 1
  //    ////          val index = LazyBSTrees.insert(l, n)
  //    //          nextInd = testTuples.next(nextInd)
  //    //        }
  //    //        nextInd shouldBe testTuples.size
  //
  //    //        withClue(testTuples.toList.map(tupleToString).mkString("\n")) {
  //    //          count shouldBe < (15)
  //    ////        }
  //    //    
  //  }

  type Ugly = //split.ChainFiniteSingleCombine[(Int, Int), LazyTuple2[Tree, Tree], Tree] 
  EnumType[Tree]

  test("insert into a tree") {
    implicit def parTrans(p: (Int, Range)) =
      (p._1, p._2, null)

    val dEnum: DepEnumType[((Int, Range), Ugly), Tree] = consMess(ms, treeTag)
    //    
    //        testTuples.size shouldBe 15

    val n: Ugly = null

    var count = 0
    for (i <- 1 to 3) {
      var nextInd = 0
      val enum = dEnum((i, 1 to i), n)
      while (nextInd < enum.size) {
        enum.reset
        val t = enum(nextInd)
        nextInd = enum.next(nextInd)
        count += 1
      }
    }
    count shouldBe 6

    {
      var count = 0
      for (i <- 1 to 3) {
        var nextInd = 0
        val enum = dEnum((i, 1 to i), n)
        while (nextInd < enum.size) {
          enum.reset
          val t = enum(nextInd)
          t match {
            case Leaf =>
            case n: Node =>
              n.v
          }
          nextInd = enum.next(nextInd)
          count += 1
        }
      }
      count shouldBe 6
    }

    {
      for (i <- 1 to 3) {
        var nextInd = 0
        val enum = dEnum((i, 1 to i), n)
        enum.reset
        val t = enum(0)
        t match {
          case Leaf =>
          case n: Node =>
            val bv = n.v
            n.l
        }
      }
    }
    {
      var count = 0
      for (i <- 1 to 3) {
        var nextInd = 0
        val enum = dEnum((i, 1 to i), n)
        while (nextInd < enum.size) {
          enum.reset
          val t = enum(nextInd)
          t match {
            case Leaf =>
            case n: Node =>
              val a = n.v
              n.l
          }
          val newInd = enum.next(nextInd)
          newInd should be > (nextInd)
          nextInd = newInd
          count += 1
        }
      }
      count shouldBe 7
    }
    {
      var lzyAll = List[Tree]()
      var count = 0
      for (i <- 3 to 3) {
        var nextInd = 0
        val enum = dEnum((i, 1 to i), n)
        while (nextInd < enum.size) {
          enum.reset
          println("reset")
          val t = enum(nextInd)
          t match {
            case Leaf =>
            case n: Node =>
              n.l != n.r
              lzyAll = n :: lzyAll
          }
          val newInd = enum.next(nextInd)
          newInd should be > (nextInd)
          nextInd = newInd
          count += 1
        }
      }

      count shouldBe 5
    }
  }

  test("correctness of iterative enumeration") {

    val enum: DepEnumType[((Int, Range), Ugly), Tree] = consMess(ms, treeTag)
    val n: Ugly = null
//    implicit def parTrans(p: (Int, Range)): ((Int, Range), Ugly) =
//      (p._1, p._2, n)

    val itEnum1 = enum((1, 1 to 1), n)
    val itEnum2 = enum((2, 1 to 2), n)

    {
      val itEnum = enum((3, 1 to 3), n)
      // after three times you should have hasNext == false
      itEnum.size shouldBe 5
      itEnum.reset
      itEnum(0)
      itEnum.next(0) shouldBe 2
      itEnum.reset
      itEnum(2)
      itEnum.next(2) shouldBe 3
      itEnum.reset
      itEnum(3)
      itEnum.next(3) shouldBe 5
    }

    {
      val itEnum = enum((3, 1 to 3), n)
      itEnum.size shouldBe 5
      itEnum.reset
      itEnum(0)
      itEnum(0) shouldBe a[Node]
      itEnum(0).asInstanceOf[Node].l
      itEnum(0).asInstanceOf[Node].r
      itEnum.next(0) shouldBe 1
      itEnum.reset
      itEnum(1)
      itEnum.next(0) shouldBe 2
      itEnum.reset
      itEnum(2)
      itEnum.next(2) shouldBe 3
      itEnum.reset
      itEnum(3)
      itEnum.next(3) shouldBe 5
    }

    {
      val size = 1;
      withClue(s"size=$size") {
        val itEnum = enum((size, 1 to size), n)
        itEnum shouldBe a[e.WrapArray[_]]
        var nextInd = 0
        while (nextInd < itEnum.size) {
          //          println(s"nextInd=$nextInd")
          itEnum.reset
          itEnum(nextInd)
          //          println(s"PAASS")
          val newInd = itEnum.next(nextInd)
          newInd shouldBe nextInd + 1
          nextInd = newInd
        }
      }
    }
    for (size <- 2 to 6) {
      //      val size = 4;
      withClue(s"size=$size") {
        val itEnum = enum((size, 1 to size), n)
        //        itEnum shouldBe a [lazytraversal.ChainFiniteSingleCombine[_, _, _]]
        var nextInd = 0
        var ind = 0
        while (ind < size) {
          //          println(s"nextInd=$nextInd")
          itEnum.reset
          itEnum(nextInd)
          val newInd = itEnum.next(nextInd)
          //          newInd shouldBe nextInd + 1
          nextInd = newInd
          ind += 1
        }
        nextInd shouldBe itEnum.size
      }
    }

  }

  test("correctness of exhaustive iterative enumeration") {
    val denum: DepEnumType[((Int, Range), Ugly), Tree] = consMess(ms, treeTag)
    val n: Ugly = null
    val dbase = constructStrict

    for (size <- 1 to 6) {
      //      val size = 3
      val range = 1 to size

      val enum = denum(((size, range), null))
      val base = dbase(size, range)

      enum.size shouldBe base.size
      enum.size shouldBe base.size
      enum.size shouldBe base.size
      var nextInd = 0
      //        val all =
      //          for (i <- 0 until enum.size) yield {
      //            info("-------------will evaluate")
      //            nextInd shouldBe < (enum.size)
      //            val v = (enum(nextInd): BSTrees.Tree)
      //
      //            val newInd = enum.next(nextInd)
      //
      //            newInd shouldBe > (nextInd)
      //
      //            nextInd = newInd
      //
      //            v
      //          }
      var all = List.empty[BSTrees.Tree]

      while (nextInd < enum.size) {
        info("-------------will evaluate")
        nextInd shouldBe <(enum.size)
        enum.reset
        val v = (enum(nextInd): BSTrees.Tree)
        //            println(s"done conversion, $v")
        all = v :: all

        val newInd = enum.next(nextInd)

        newInd shouldBe >(nextInd)

        nextInd = newInd
        //          println(s"nextInd=$nextInd")

        v
      }
      lazy val clue = s"all=\n${all.mkString("\n")}\n base=\n${base.toList.mkString("\n")}"
      //              lazy val clue = s"all-base=${all.toSet.diff(base.toList.toSet)}" +
      //                s"base-all=${base.toSet.diff(all.toList.toSet)}" +
      //                s"notgenind=${base.toList.indexOf(base.toSet.diff(all.toList.toSet).head)}/ ${base.toList.size}"
      //          
      withClue(clue) {
        all should contain theSameElementsAs (base.toList)
      }
    }

  }

  //  //
  //  test("parts of enumerator") {
  //
  //    val enum =
  //      new e.WrapArray(1 to 1 map { v => (Node(Leaf, v, Leaf): Tree) } toArray) with Touchable[Tree] with NoSkip[Tree]
  //
  //    enum(0)
  //    enum.next(0) shouldBe 1
  //    enum.size shouldBe 1
  //    //    enum(1)
  //    //    enum.next(1) shouldBe 2
  //  }
  //  //
  //    test("correctness of iterative enumeration") {
  //  
  //      val enum = constructEnumerator
  //      val itEnum1 = enum(1, 1 to 1)
  //      val itEnum2 = enum(2, 1 to 2)
  //  
  //      {
  //        val itEnum = enum(3, 1 to 3)
  //        // after three times you should have hasNext == false
  //        itEnum.size shouldBe 5
  //        itEnum.reset
  //        itEnum(0)
  //        itEnum.next(0) shouldBe 2
  //        itEnum.reset
  //        itEnum(2)
  //        itEnum.next(2) shouldBe 3
  //        itEnum.reset
  //        itEnum(3)
  //        itEnum.next(3) shouldBe 5
  //      }
  //  
  //      {
  //        val itEnum = enum(3, 1 to 3)
  //        itEnum.size shouldBe 5
  //        itEnum.reset
  //        itEnum(0)
  //        itEnum(0) shouldBe a[Node]
  //        itEnum(0).asInstanceOf[Node].l
  //        itEnum(0).asInstanceOf[Node].r
  //        itEnum.next(0) shouldBe 1
  //        itEnum.reset
  //        itEnum(1)
  //        itEnum.next(0) shouldBe 2
  //        itEnum.reset
  //        itEnum(2)
  //        itEnum.next(2) shouldBe 3
  //        itEnum.reset
  //        itEnum(3)
  //        itEnum.next(3) shouldBe 5
  //      }
  //  
  //      {
  //        val size = 1;
  //        withClue(s"size=$size") {
  //          val itEnum = enum(size, 1 to size)
  //          itEnum shouldBe a[e.WrapArray[_]]
  //          var nextInd = 0
  //          while (nextInd < itEnum.size) {
  //            //          println(s"nextInd=$nextInd")
  //        itEnum.reset
  //            itEnum(nextInd)
  //  //          println(s"PAASS")
  //            val newInd = itEnum.next(nextInd)
  //            newInd shouldBe nextInd + 1
  //            nextInd = newInd
  //          }
  //        }
  //      }
  //      for (size <- 2 to 6) {
  //        //      val size = 4;
  //        withClue(s"size=$size") {
  //          val itEnum = enum(size, 1 to size)
  //          //        itEnum shouldBe a [lazytraversal.ChainFiniteSingleCombine[_, _, _]]
  //          var nextInd = 0
  //          var ind = 0
  //          while (ind < size) {
  //            //          println(s"nextInd=$nextInd")
  //        itEnum.reset
  //            itEnum(nextInd)
  //            val newInd = itEnum.next(nextInd)
  //            //          newInd shouldBe nextInd + 1
  //            nextInd = newInd
  //            ind += 1
  //          }
  //          nextInd shouldBe itEnum.size
  //        }
  //      }
  //  
  //    }
  //  
  //    test("correctness of exhaustive iterative enumeration") {
  //      val denum = constructEnumerator
  //      val dbase = constructStrict
  //  
  //            for (size <- 1 to 6)
  //      {
  //  //      val size = 3
  //        val range = 1 to size
  //  
  //        val enum = denum((size, range))
  //        val base = dbase(size, range)
  //  
  //        enum.size shouldBe base.size
  //        enum.size shouldBe base.size
  //        enum.size shouldBe base.size
  //        var nextInd = 0
  //        //        val all =
  //        //          for (i <- 0 until enum.size) yield {
  //        //            info("-------------will evaluate")
  //        //            nextInd shouldBe < (enum.size)
  //        //            val v = (enum(nextInd): BSTrees.Tree)
  //        //
  //        //            val newInd = enum.next(nextInd)
  //        //
  //        //            newInd shouldBe > (nextInd)
  //        //
  //        //            nextInd = newInd
  //        //
  //        //            v
  //        //          }
  //        var all = List.empty[BSTrees.Tree]
  //          
  //          while (nextInd < enum.size) {
  //            info("-------------will evaluate")
  //            nextInd shouldBe <(enum.size)
  //        enum.reset
  //            val v = (enum(nextInd): BSTrees.Tree)
  ////            println(s"done conversion, $v")
  //            all = v :: all
  //  
  //            val newInd = enum.next(nextInd)
  //  
  //            newInd shouldBe >(nextInd)
  //  
  //            nextInd = newInd
  //  //          println(s"nextInd=$nextInd")
  //  
  //            v
  //          }
  //              lazy val clue = s"all=\n${all.mkString("\n")}\n base=\n${base.toList.mkString("\n")}"
  //  //              lazy val clue = s"all-base=${all.toSet.diff(base.toList.toSet)}" +
  //  //                s"base-all=${base.toSet.diff(all.toList.toSet)}" +
  //  //                s"notgenind=${base.toList.indexOf(base.toSet.diff(all.toList.toSet).head)}/ ${base.toList.size}"
  //        //          
  //                withClue (clue) {
  //        all should contain theSameElementsAs (base.toList)
  //                }
  //      }
  //  
  //    }

  //    test("correctness of exhaustive iterative enumeration, next") {
  //      val enum = constructEnumerator
  //  
  //      for (size <- 0 to 5) {
  //        val itEnum = enum(size, 1 to size)
  //        while (itEnum.hasNext) itEnum.next
  //      }
  //  
  //    }
  //  
  //    test("correctness of exhaustive iterative enumeration, next, reset") {
  //      val enum = constructEnumerator
  //      val base = constructStrict
  //  
  //      for (size <- 0 to 7) {
  //        val range = 1 to size
  //        val itEnum = enum(size, range)
  //        val baseEnum = base( (size, range) )
  //  
  //        for (_ <- 1 to 3) {
  //          itEnum.size shouldBe baseEnum.size
  //  
  //          val all =
  //          for (i <- 0 until baseEnum.size) yield {
  //            itEnum.hasNext shouldBe true
  //            itEnum.next: BSTrees.Tree
  //          }
  //          baseEnum.toList should contain theSameElementsAs all
  //  
  //          itEnum.hasNext shouldBe false
  //          itEnum.reset
  //        }
  //      }
  //  
  //    }
  //  
  //    ignore("resetting enumerator in the middle, revisit this") {
  //      val depEnum = constructEnumerator
  //      val enum = depEnum(6, 1 to 6)
  //  
  //      val halfOfTheEnum =
  //        for (ind <- 0 until enum.size / 2)
  //          yield enum.next
  //  
  //      enum.reset
  //  
  //      for ((el, ind) <- halfOfTheEnum.zipWithIndex)
  //        enum.next should be(el)
  //  
  //    }
  trait Print[A] extends Finite[A] {
    abstract override def apply(i: Int) = {
      println(s"apply at $this")
      super.apply(i)
    }
  }

  def constructEnumerator2(implicit ms: e.memoization.MemoizationScope) = {
    new WrapFunctionTest[(Int, Range), Tree, EnumType](
      ((self: DepEnumType[(Int, Range), Tree], pair: (Int, Range)) => {
        val (size, range) = pair

        if (size < 0) throw new RuntimeException
        if (size <= 0) new e.Singleton((Leaf: Tree)) with Touchable[Tree] with Resetable[Tree] with NoSkip[Tree] {
          override def toString = s"Singleton[$hashCode]"
        }
        else if (range.isEmpty) Empty
        else if (size == 1)
          new e.WrapArray(range map { v => (Node(Leaf, v, Leaf): Tree) } toArray) with Touchable[Tree] with Resetable[Tree] with NoSkip[Tree] {

            //            override def toString = s"Array[$hashCode](${toList})"
            override def toString = s"Array[$hashCode]()"
          }
        else {
          val leftSizes = e.Enum(0 until size)
          val roots = e.Enum(range)

          val rootLeftSizePairs = e.Product(leftSizes, roots)

          val leftTrees: DepEnumTypeFinite[(Int, Int), Tree] = new InMap(self, { (par: (Int, Int)) =>
            val (leftSize, median) = par
            (leftSize, range.start to (median - 1))
          }) with DependFinite[(Int, Int), Tree] {
            override type EnumSort[A] = SortedTreeTest.this.EnumType[A]
          }

          val rightTrees: DepEnumTypeFinite[(Int, Int), Tree] =
            new InMap(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              (size - leftSize - 1, (median + 1) to range.end)
            }) with DependFinite[(Int, Int), Tree] {
              override type EnumSort[A] = SortedTreeTest.this.EnumType[A]
            }

          val leftRightPairs: DepEnumTypeFinite[(Int, Int), LazyTuple2[Tree, Tree]] =
            lazytraversal.dependent.ProductFinite(leftTrees, rightTrees)

          val fConstructTree: ((Int, Int), => LazyTuple2[Tree, Tree]) => Tree =
            (p1, p2) => {
              //              print(s"map invoked: ${p1}")
              Node(p2._1, p1._2, p2._2)
            }

          val allNodes =
            new lazytraversal.ChainFiniteSingleCombine[(Int, Int), LazyTuple2[Tree, Tree], Tree](
              rootLeftSizePairs, leftRightPairs,
              fConstructTree) with Touchable[Tree] {
              override def toString = s"ChainFiniteSingleCombine[$hashCode]"
            }

          allNodes: SortedTreeTest.this.EnumType[Tree]
        }
      }): (DepEnumType[(Int, Range), Tree], (Int, Range)) => EnumType[Tree])
  }

//  def constructEnumerator_memDoesNotWork(implicit ms: e.memoization.MemoizationScope, tt: reflect.ClassTag[Tree]) = {
//    new WrapFunctionTest[(Int, Range), Tree, EnumType](
//      ((self: DepEnumType[(Int, Range), Tree], pair: (Int, Range)) => {
//        val (size, range) = pair
//
//        if (size < 0) throw new RuntimeException
//        if (size <= 0) new e.Singleton((Leaf: Tree)) with Touchable[Tree] with Resetable[Tree] with NoSkip[Tree] {
//          override def toString = s"Singleton[$hashCode]"
//        }
//        else if (range.isEmpty) Empty
//        else if (size == 1)
//          new e.WrapArray(range map { v => (Node(Leaf, v, Leaf): Tree) } toArray) with Touchable[Tree] with Resetable[Tree] with NoSkip[Tree] {
//
//            //            override def toString = s"Array[$hashCode](${toList})"
//            override def toString = s"Array[$hashCode]()"
//          }
//        else {
//          val leftSizes = e.Enum(0 until size)
//          val roots = e.Enum(range)
//
//          val rootLeftSizePairs = e.Product(leftSizes, roots)
//
//          val leftTrees: DepEnumTypeFinite[(Int, Int), Tree] = new InMap(self, { (par: (Int, Int)) =>
//            val (leftSize, median) = par
//            (leftSize, range.start to (median - 1))
//          }) with DependFinite[(Int, Int), Tree] {
//            override type EnumSort[A] = SortedTreeTest.this.EnumType[A]
//          }
//
//          val rightTrees: DepEnumTypeFinite[(Int, Int), Tree] =
//            new InMap(self, { (par: (Int, Int)) =>
//              val (leftSize, median) = par
//              (size - leftSize - 1, (median + 1) to range.end)
//            }) with DependFinite[(Int, Int), Tree] {
//              override type EnumSort[A] = SortedTreeTest.this.EnumType[A]
//            }
//
//          val leftRightPairs: DepEnumTypeFinite[(Int, Int), LazyTuple2[Tree, Tree]] =
//            lazytraversal.dependent.ProductFinite(leftTrees, rightTrees)
//
//          val fConstructTree: ((Int, Int), => LazyTuple2[Tree, Tree]) => Tree =
//            (p1, p2) => {
//              //              print(s"map invoked: ${p1}")
//              Node(p2._1, p1._2, p2._2)
//            }
//
//          val allNodes =
//            new {
//              val classTagT = treeTag
//            } with lazytraversal.ChainFiniteSingleCombine[(Int, Int), LazyTuple2[Tree, Tree], Tree](
//              rootLeftSizePairs, leftRightPairs,
//              fConstructTree) with e.memoization.MemoizedSize with e.memoization.MemoizedStatic[Tree] with Touchable[Tree] {
//              //            override def toString = s"ChainFiniteSingleCombine[$hashCode](${leftRightPairs.hashCode})"
//            }
//
//          allNodes: SortedTreeTest.this.EnumType[Tree]
//        }
//      }): (DepEnumType[(Int, Range), Tree], (Int, Range)) => EnumType[Tree]) with e.memoization.dependent.Memoized[(Int, Range), Tree]
//  }

  //  def constructEnumerator(implicit ms: e.memoization.MemoizationScope, tt: reflect.ClassTag[Tree]) =
  //    new InMap(consMess(ms, tt), { (par: (Int, Range)) =>
  //              (par._1, par._2, null)
  //            }) with DependFinite[(Int, Int), Tree] {
  //            override type EnumSort[A] = SortedTreeTest.this.EnumType[A]
  //          }

  def consMess(implicit ms: e.memoization.MemoizationScope, tt: reflect.ClassTag[Tree]) = {
    new WrapFunctionTest2[((Int, Range), Ugly), Tree, EnumType](
      ((self: DepEnumType[((Int, Range), Ugly), Tree], pair: ((Int, Range), Ugly)) => {
        val ((size, range), ug) = pair

        val reuse: split.ChainFiniteSingleCombine[(Int, Int), LazyTuple2[Tree, Tree], Tree] =
          if (ug.isInstanceOf[split.ChainFiniteSingleCombine[(Int, Int), LazyTuple2[Tree, Tree], Tree]]) {
            ug.asInstanceOf[split.ChainFiniteSingleCombine[(Int, Int), LazyTuple2[Tree, Tree], Tree]]
          } else null

        if (size < 0) throw new RuntimeException
        if (size <= 0) new e.Singleton((Leaf: Tree)) with Touchable[Tree] with Resetable[Tree] with NoSkip[Tree] {
          override def toString = s"Singleton[$hashCode]"
        }
        else if (range.isEmpty) Empty
        else if (size == 1)
          new e.WrapArray(range map { v => (Node(Leaf, v, Leaf): Tree) } toArray) with Touchable[Tree] with Resetable[Tree] with NoSkip[Tree] {

            //            override def toString = s"Array[$hashCode](${toList})"
            override def toString = s"Array[$hashCode]()"
          }
        else {
          val leftSizes = e.Enum(0 until size)
          val roots = e.Enum(range)

          val rootLeftSizePairs = e.Product(leftSizes, roots)

          val leftTrees: DepEnumTypeFinite[(Int, Int), Tree] = new InMap(self, { (par: (Int, Int)) =>
            val (leftSize, median) = par
            ((leftSize, range.start to (median - 1)), ug)
          }) with DependFinite[(Int, Int), Tree] {
            override type EnumSort[A] = SortedTreeTest.this.EnumType[A]
          }

          val rightTrees: DepEnumTypeFinite[(Int, Int), Tree] =
            new InMap(self, { (par: (Int, Int)) =>
              val (leftSize, median) = par
              ((size - leftSize - 1, (median + 1) to range.end), ug)
            }) with DependFinite[(Int, Int), Tree] {
              override type EnumSort[A] = SortedTreeTest.this.EnumType[A]
            }

          val leftRightPairs: DepEnumTypeFinite[(Int, Int), LazyTuple2[Tree, Tree]] =
            lazytraversal.split.dependent.ProductFinite(leftTrees, rightTrees)

          val fConstructTree: ((Int, Int), => LazyTuple2[Tree, Tree]) => Tree =
            (p1, p2) => {
              Node(p2._1, p1._2, p2._2)
            }

          val allNodes =
            if (reuse == null)
              new {
                val classTagT = treeTag
              } with lazytraversal.split.ChainFiniteSingleCombine[(Int, Int), LazyTuple2[Tree, Tree], Tree](
                rootLeftSizePairs, leftRightPairs,
                fConstructTree)(null) with e.memoization.MemoizedSize with e.memoization.MemoizedStatic[Tree] with Touchable[Tree] {
                //            override def toString = s"ChainFiniteSingleCombine[$hashCode](${leftRightPairs.hashCode})"
              }
            else
              new {
                val classTagT = treeTag
              } with lazytraversal.split.ChainFiniteSingleCombine[(Int, Int), LazyTuple2[Tree, Tree], Tree](
                rootLeftSizePairs, leftRightPairs,
                fConstructTree)(reuse.inner) with e.memoization.MemoizedSize with e.memoization.MemoizedStatic[Tree] with Touchable[Tree] {
                //            override def toString = s"ChainFiniteSingleCombine[$hashCode](${leftRightPairs.hashCode})"
              }

          allNodes: SortedTreeTest.this.EnumType[Tree]
        }
      }): (DepEnumType[((Int, Range), Ugly), Tree], ((Int, Range), Ugly)) => EnumType[Tree]) with split.Memoized[(Int, Range), Tree, Ugly]
  }

}
