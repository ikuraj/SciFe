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

// here, the sizes of leaves (i.e. variables) is considered to be 0
class ClosedLambdaAltTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with HasLogger with ProfileLogger {

  import Checks._
  import Util._
  import Common._
  
  trait LambdaTerm
  case class Var(id: Int) extends LambdaTerm
  case class Application(f: LambdaTerm, p: LambdaTerm) extends LambdaTerm
  case class Lambda(id: Int, b: LambdaTerm) extends LambdaTerm

  // input: (size, available, declared)
  type Input = (Int, Set[Int], Int)
  type Output = LambdaTerm
  type EnumType = Depend[Input, Output]

  test("enumeration") {
    val checkerHelper = new CheckerHelper[Output]
    import checkerHelper._

      def rangeList(m: Int) = m to 0 by -1 toArray
    val enum = constructEnumerator

    withLazyClue("Elements are: " + clue) {

      res = enum.getEnum((1, Set(1), 0))
      res.size should be(3)
      
      res = enum.getEnum((2, Set(0), 0))
      res.size should be(7)
      
      for ((numOfTerms, size) <-
        List(0, 1, 3, 14, 82, 579, 4741, 43977) zipWithIndex) {
        withClue("For size: " + size) {
          res = enum.getEnum((size, Set(), 0))
          res.size should be (numOfTerms)
        }
      }

    }

  }

  //  val subListChooser: DependFinite[(Int, List[Int]), List[Int]] = Depend.memoizedFin(
  //    (self: DependFinite[(Int, List[Int]), List[Int]], pair: (Int, List[Int])) => {
  //      val (size, range) = pair
  //
  //      if (size <= 0) e.Singleton(Nil): Finite[List[Int]]
  //      else if (size == 1) e.Enum(range map { List(_) }): Finite[List[Int]]
  //      else if (size <= range.size) {
  //        val temp = self.getEnum((size - 1, range.tail))
  //        val kept = Map(temp, { range.head :: (_: List[Int]) })
  //        val leftOut = self.getEnum((size, range.tail))
  //
  //        val allNodes = e.Concat(kept, leftOut)
  //        allNodes: Finite[List[Int]]
  //      } else e.Empty: Finite[List[Int]]
  //    })

  def constructEnumerator(implicit ms: MemoizationScope = null) = {

    Depend.memoizedFin[Input, Output](
      (self: DependFinite[Input, Output], par: Input) => {
        val (size, available, declared) = par

        if (size == 0 && available.size < 1) Empty
        else if (size == 0) {
          Enum(available.toArray) map { (id: Int) => Var(id) }: Finite[Output]
        } else {
          val lambdas: Finite[Output] =
            self(size - 1, available + declared, declared + 1) map {
              body => Lambda(declared, body)
            }

          val subtreesLeft =
            InMap(self, { (lSize: Int) =>
              (lSize, available, declared)
            })

          val subtreesRight =
            InMap(self, { (lSize: Int) =>
              (size - lSize - 1, available, declared)
            })

          val applications: Finite[Output] =
            memoization.Chain.fin(
              e.WrapArray(0 to size - 1),
              e.dependent.Product(subtreesLeft, subtreesRight)) map {
                (r: (Int, (Output, Output))) =>
                  val (chosen, (lTree, rTree)) = r
                  Application(lTree, rTree)
              }

          (lambdas concat applications): Finite[Output]
        }
      })
  }

}
