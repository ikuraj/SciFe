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

class BTreeTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
  HasLogger with ProfileLogger {

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

    withLazyClue("Elements are: " + clue) {
      
      var res = enum(1, 1 to 1, 1)
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
      res.size shouldBe 0

      res = enum(4, 1 to 4, 1)
      res.size shouldBe 0
//      res = enum(4, 1 to 4, 2)
//      res.size shouldBe 4
      
    }

  }

  def getAdditionsEnum(implicit ms: MemoizationScope = null): DependFinite[(Int, Int, Int), List[Int]] = {
    type Input = (Int, Int, Int)
    
    Depend.memoizedFin(
      (self: DependFinite[Input, List[Int]], pair: Input) => {
        val (size, amount, max) = pair 
        
        if (size == 0) e.Singleton(Nil)
        else {
          e.dependent.Chain.single(Enum(0 to math.max(amount, max)),
            Depend.fin({ (added: Int) =>
              self(size - 1, amount - added, max) map {
                added :: _
              }
          } ))  
        }
      }
    )
  }

  def constructEnumerator(t: Int)(implicit ms: MemoizationScope = null, ct: ClassTag[Output]) = {
    
    val additionsEnum = getAdditionsEnum(ms)

    Depend.memoizedFin(
      (self: DependFinite[Input, Output], pair: Input) => {
        val (size: Int, keyRange: Range, h: Int) = pair
        
        def minChildSize = math.pow(2 * t, h - 1).toInt - 1
        def maxChildSize = math.pow(2 * t, h).toInt - 1

        if (size <= 0 || h <= 0) throw new RuntimeException
        else if (h == 1 && size == keyRange.size && size < 2 *t) {
          e.Singleton( Tree(keyRange.toList, Nil) ): Finite[Output]
        }
        else if (h > 1 && size == keyRange.size) {
          val nChildrenEnum = e.Enum(t to 2 * t)

          val rootLeftPairs2: Finite[Output] = e.dependent.Chain.single(nChildrenEnum,
            Depend.fin({ (nChildren: Int) =>
              val nKeys = nChildren - 1
              val restOfNodes: Int = size - nKeys - nChildren * minChildSize
//              restOfNodes shouldBe > (0)
              
              if (restOfNodes < 0) {
                info(s"input=$pair, restOfNodes=$restOfNodes")
                e.Empty
              }
              else
              e.dependent.Chain.single[List[Int], Output](
                 additionsEnum(nChildren, restOfNodes, maxChildSize - minChildSize): Finite[List[Int]],
                 Depend.fin({ (addList: List[Int]) =>
                   val childSizes = addList map { _ + minChildSize }
                   childSizes.size shouldBe nChildren

                   val keys: List[Int] = (childSizes.scanLeft(keyRange.start) {
                     case (soFar, el) => soFar + el
                   }).tail.tail;
                   keys.size shouldBe nKeys
                   
                   val childRanges =
                     ((keyRange.start :: keys) zip (keyRange.tail :+ keyRange.end)) map {
                       case (a, b) => (a + 1) to (b - 1)
                     }
                   info(s"childRanges=$childRanges")
                   childRanges.size shouldBe nChildren

                   childSizes.size shouldBe childRanges.size
                   val childEnums: Array[Finite[Output]] = (childSizes zip childRanges) map {
                     case (cs, cr) => self(cs, cr, h-1)
                   } toArray;
                   childEnums.size shouldBe nChildren
                   
                   e.Product.fin(childEnums) map {
                     children =>
                       children.size shouldBe keys.size + 1
                       Tree(keys, children)
                   }
                 }): DependFinite[List[Int], Output]
              )
          } ))
          
          rootLeftPairs2: Finite[Output]
        } else {
          info(s"input=$pair")
          e.Empty //throw new RuntimeException(s"input=$pair")
        }
      })

  }
//
//  val listOfNodes: Depend[(Int, Int, Int), List[Tree]] = {
//
//    Depend.memoized(
//      (self: Depend[(Int, Int, Int), List[Tree]], pair: (Int, Int, Int)) => {
//        val (size: Int, nChildren: Int, h: Int) = pair
//        
//        val minPerSubtree = math.pow(2 * t, h) - 1
//
//        if (size <= 0 || h <= 0) throw new RuntimeException
////        else if (h == 1) {
////          e.Singleton( Tree(List.fill(size)(1), Nil) :: Nil )
////        }
//        
//          val minSize = math.pow(2 * t, h).toInt - 1
//          val restOfNodes: Int = size - nChildren * minSize
//          val toAddEnum: Enum[Int] = e.Enum(0 to restOfNodes)
//          
//          val nodes: Enum[List[Tree]] = e.dependent.Chain.single(toAddEnum,
//            Depend({ (toAdd: Int) =>
//              val nodesEnum = constructEnumerator()(minSize + toAdd, 1 to 1, h)
//              val restOfListEnum = self(size - minSize - toAdd, nChildren - 1, h)
//
//              e.Product(nodesEnum, restOfListEnum) map { (p: (Tree, List[Tree])) =>
//                val (root, rest) = p
//                root :: rest
//              }: Enum[List[Tree]]
//          } ))
//          
//          nodes
//        
//      })
//
//  }

}
