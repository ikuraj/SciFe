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

import scala.language.postfixOps
import scala.language.existentials

class BTreeTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
  HasLogger with ProfileLogger {

  import Checks._
  import structures._
  import BTree._

  import Util._
  import Common._
  
  val t = 2

  // size, range of keys
  type Input = (Int, Range, Int)
  type Output = BTree.Tree
  type EnumType = Depend[Input, Output]

  test("enumeration") {
    val checkerHelper = new CheckerHelper[Output]
    import checkerHelper._

    val enum = constructEnumerator

    withLazyClue("Elements are: " + clue) {
      
    }

  }

  def constructEnumerator(implicit ms: MemoizationScope = null) = {

    Depend.memoized(
      (self: Depend[Input, Output], pair: Input) => {
        val (size: Int, keyRange: Range, h: Int) = pair
        
        val minPerSubtree = math.pow(2 * t, h) - 1

        if (size <= 0 || h <= 0) throw new RuntimeException
        else if (h == 1) {
          e.Singleton( Tree(List.fill(size)(1), Nil) ): Enum[Output]
        }
        else {
          val nChildrenEnum = e.Enum(t to 2 * t)
          val rootLeftPairs2: Enum[Output] = e.dependent.Chain.single(nChildrenEnum,
            Depend({ (nChildren: Int) =>
              val nKeysThis = nChildren - 1
              val keys = List.fill(nKeysThis)(1)

              listOfNodes(size - nKeysThis, nChildren, h -1) map {
                Tree(keys, _)
              }
          } ))
          
          rootLeftPairs2
        }
      })

  }

  val listOfNodes: Depend[(Int, Int, Int), List[Tree]] = {

    Depend.memoized(
      (self: Depend[(Int, Int, Int), List[Tree]], pair: (Int, Int, Int)) => {
        val (size: Int, nChildren: Int, h: Int) = pair
        
        val minPerSubtree = math.pow(2 * t, h) - 1

        if (size <= 0 || h <= 0) throw new RuntimeException
//        else if (h == 1) {
//          e.Singleton( Tree(List.fill(size)(1), Nil) :: Nil )
//        }
        
          val minSize = math.pow(2 * t, h).toInt - 1
          val restOfNodes: Int = size - nChildren * minSize
          val toAddEnum: Enum[Int] = e.Enum(0 to restOfNodes)
          
          val nodes: Enum[List[Tree]] = e.dependent.Chain.single(toAddEnum,
            Depend({ (toAdd: Int) =>
              val nodesEnum = constructEnumerator()(minSize + toAdd, 1 to 1, h)
              val restOfListEnum = self(size - minSize - toAdd, nChildren - 1, h)

              e.Product(nodesEnum, restOfListEnum) map { (p: (Tree, List[Tree])) =>
                val (root, rest) = p
                root :: rest
              }: Enum[List[Tree]]
          } ))
          
          nodes
        
      })

  }

}
