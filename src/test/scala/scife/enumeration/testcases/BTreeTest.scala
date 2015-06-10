package scife.enumeration
package testcases

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._

import dependent._
import scife.{ enumeration => e }
import memoization._

import scife.util._
import scife.util.logging._
import structures.BSTrees._

import org.scalatest._
import org.scalameter.api._

import scala.reflect._
import scala.language.postfixOps
import scala.language.existentials

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class BTreeTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {

  import Checks._
  import structures._
  import BTree._

  import Util._
  import Common._

  // size, range of keys
  type Input = (Int, Range, Int)
  type Output = BTree.Tree
  type EnumType = Depend[Input, Output]

  test("enumeration") {

    val checkerHelper = new CheckerHelper[Output]
    import checkerHelper._

    val enum = constructEnumerator(2)
    var res = enum(1, 1 to 1, 1)
    def clue = res mkString "\n"

    withLazyClue("Elements are:\n" + clue) {

      res = enum(1, 1 to 1, 1)
      res.size shouldBe 1

      //      res = enum(1, 0 to 1, 1)
      //      res.size shouldBe 2

      res = enum(2, 1 to 2, 1)
      res.size shouldBe 1

      //      res = enum(2, 0 to 2, 1)
      //      res.size shouldBe 3

      res = enum(3, 1 to 3, 1)
      res.size shouldBe 1
      res = enum(3, 1 to 3, 2)
      res.size shouldBe 1

      res = enum(4, 1 to 4, 1)
      res.size shouldBe 0
      res = enum(4, 1 to 4, 2)
      res.size shouldBe 2

      res = enum(5, 1 to 5, 1)
      res.size shouldBe 0
      res = enum(5, 1 to 5, 2)
      res.size shouldBe 4

      res = enum(6, 1 to 6, 1)
      res.size shouldBe 0
      res = enum(6, 1 to 6, 2)
      res.size shouldBe 5
      
      res = enum(3, 1 to 4, 1)
      res.size shouldBe 4
      res = enum(3, 1 to 4, 2)
      res.size shouldBe 4

      res = enum(3, 1 to 5, 1)
      res.size shouldBe 10
      res = enum(3, 1 to 5, 2)
      res.size shouldBe 10
      res = enum(3, 1 to 5, 3)
      res.size shouldBe 0
    }

    for ((size, count) <- List(
      (1, 1), (2, 3)
      ,(3, 8),
      (4, 20)
      ,(5, 49)
      ,(6, 120)
      ,(10, 4853)
      ,(12, 33971)
    )) {
      val range = 1 to size

      val allEl =
        (for (h <- 1 to (math.log(size+1).toInt + 1)) yield {
          //val newE = e.dependent.Chain.single(Enum((1 to size).toArray) map { x => (x, range, h) }, enum)
          (for (s <- 1 to size) yield  {
          val newE = enum(s, range, h)

          info(s"(s, r, h)=${(s, range, h)}, got ${newE.toList.mkString("\n")}")
          newE.toList
          }) flatten
        }) flatten

      withLazyClue("Elements are:\n" + allEl.mkString("\n")) {
        allEl.size shouldBe count
      }
    }

  }

  test("additions") {
    val enum = getAdditionsEnum

    var res = enum(3, 6, 5)
    res.size shouldBe 25

    res.toList should contain allOf (
      List(3, 3, 0),
      List(2, 2, 2),
      List(1, 2, 3),
      List(0, 5, 1))
    res.toList should not contain List(
      List(1, 3, 1),
      List(0, 3, 4),
      List(6, 0, 2),
      List(6, 0, 2),
      List(6, 0, 0))

    res = enum(4, 0, 3)
    res.size shouldBe 1

    res = enum(2, 1, 3)
    res foreach print
    res.size shouldBe 2
  }
  
  test("sublists test") {
    var enum = e.Sublists.apply(List(1, 2, 3), 1)
    enum.size shouldBe 3
    
    enum = e.Sublists.apply(1 to 5 toList, 3)
    enum.size shouldBe 10
  }

  def getAdditionsEnum(implicit ms: MemoizationScope = null): DependFinite[(Int, Int, Int), List[Int]] = {
    type Input = (Int, Int, Int)

    Depend.memoizedFin(
      (self: DependFinite[Input, List[Int]], pair: Input) => {
        val (size, amount, max) = pair

        if (size == 0 && amount > 0) e.Empty
        else if (size == 0) e.Singleton(Nil)
        else if (size == 1 && amount > max) e.Empty
        else {
          e.dependent.Chain.single(Enum(0 to math.min(amount, max)),
            Depend.fin({ (added: Int) =>
              self(size - 1, amount - added, max) map {
                added :: _
              }
            }))
        }
      })
  }

  def constructEnumerator(t: Int)(implicit ms: MemoizationScope = null, ct: ClassTag[Output]) = {

    def minChildSize(h: Int) = 2 * math.pow(t, h - 1).toInt - 1
    def maxChildSize(h: Int) = math.pow(2 * t, h).toInt - 1

    val additionsEnum = getAdditionsEnum(ms)

    var nonRootNodeEnum: DependFinite[(Int, Range, Int), Output] = null

    val enumChildren: DependFinite[(Int, Int, Range, Int), Output] = Depend.fin({ in: (Int, Int, Range, Int) =>
      val (nChildren: Int, size: Int, keyRange: Range, h: Int) = in
      info(s"nChildren=$nChildren, range=$keyRange")

      val minChildSizeBelow = minChildSize(h - 1)
      val maxChildSizeBelow = maxChildSize(h - 1)
      val nKeys = nChildren - 1
      val restOfNodes: Int = size - nKeys - nChildren * minChildSizeBelow
      info(s"minChildSizeBelow=$minChildSizeBelow, maxChildSizeBelow=$maxChildSizeBelow, restOfNodes=$restOfNodes")
      //              restOfNodes shouldBe > (0)

      if (restOfNodes < 0) {
        info(s"returing empty due to restOfNodes=$restOfNodes (input=$in," +
          s"minChildSize(h)=${minChildSize(h)})")
        e.Empty
      } else {
        e.dependent.Chain.single[List[Int], Output](
          additionsEnum(nChildren, restOfNodes, maxChildSizeBelow - minChildSizeBelow): Finite[List[Int]],
          Depend.fin({ (addList: List[Int]) =>
            val childSizes = addList map { _ + minChildSizeBelow }
            info(s"childSizes=$childSizes")
            childSizes.size shouldBe nChildren

            e.dependent.Chain.single[List[Int], Output](
              additionsEnum(nChildren, keyRange.size - size, keyRange.size): Finite[List[Int]],
              Depend.fin({ (addKeys: List[Int]) =>
                val keys: List[Int] = ((childSizes zip addKeys).scanLeft(keyRange.start - 1) {
                  case (soFar, (childSize, add)) => soFar + childSize + add + 1
                }).tail.init
                info(s"keys=$keys")
                keys.size shouldBe nKeys
    
                val childRanges =
                  ((keyRange.start - 1 :: keys) zip (keys :+ (keyRange.end + 1))) map {
                    case (a, b) => (a + 1) to (b - 1)
                  }
                info(s"childRanges=$childRanges, zip=${(keyRange.start - 1 :: keys) zip (keyRange.tail :+ (keyRange.end + 1))}")
                childRanges.size shouldBe nChildren
    
                childSizes.size shouldBe childRanges.size
                val childEnums: Array[Finite[Output]] = (childSizes zip childRanges) map {
                  case (cs, cr) => nonRootNodeEnum(cs, cr, h - 1)
                } toArray;
                childEnums.size shouldBe nChildren
    
                e.Product.fin(childEnums) map {
                  children =>
                    children.size shouldBe keys.size + 1
                    Tree(keys, children)
                }
              }))
          }): DependFinite[List[Int], Output])
      }
    })

    nonRootNodeEnum = Depend.memoizedFin(
      (self: DependFinite[Input, Output], pair: Input) => {
        val (size: Int, keyRange: Range, h: Int) = pair

        if (h == 1 /*&& size == keyRange.size*/ && size < 2 * t && size >= t - 1) {
          //e.Singleton(Tree(keyRange.toList, Nil)): Finite[Output]
          e.Sublists(keyRange.toList, size) map { x => Tree(x.toList, Nil) }
        } else if (h > 1 /*&& size == keyRange.size*/ && size > 0) {
          val nChildrenEnum = e.Enum(t to 2 * t)

          e.dependent.Chain.single(nChildrenEnum map { x => (x, size, keyRange, h) },
            enumChildren): Finite[Output]
        } else {
          info(s"input=$pair")
          e.Empty //throw new RuntimeException(s"input=$pair")
        }
      })

    val rootNodeEnum = Depend.memoizedFin(
      (self: DependFinite[Input, Output], pair: Input) => {
        val (size: Int, keyRange: Range, h: Int) = pair

        if (h == 1 /*&& size == keyRange.size*/ && size < 2 * t) {
          //e.Singleton(Tree(keyRange.toList, Nil)): Finite[Output]
          e.Sublists(keyRange.toList, size) map { x => Tree(x.toList, Nil) }
        } else
          // uncomenting size >= 2*t will actually prevent some trees that cannot happen by insertion?
          if (h > 1 /*&& size == keyRange.size && size >= 2 * t*/) {
          val nChildrenEnum = e.Enum(2 to 2 * t)

          e.dependent.Chain.single(nChildrenEnum map { x => (x, size, keyRange, h) },
            enumChildren): Finite[Output]
        } else {
          info(s"input at root=$pair")
          e.Empty //throw new RuntimeException(s"input=$pair")
        }
      })

    rootNodeEnum
  }

}
