package insynth
package enumeration
package benchmarks

import dependent._
import insynth.{ enumeration => e }
import memoization._

import insynth.util._
import insynth.util.logging._
import structures.BSTrees._

import org.scalatest._
import org.scalameter.api._

import scala.language.postfixOps
import scala.language.existentials

class ClassInterfaceDAGBenchmark
  extends StructuresBenchmark[Depend[
  (Int, Int, Set[Int], Set[Int], List[Int], Predef.Map[Int, Set[Int]]),
  List[(Int, List[Int], List[Int], List[Int])]
  ]]
  with java.io.Serializable with HasLogger {

  // (size, Id, #class, #interface, #overridableMethods, map(node->sealed))
  type Input = (Int, Int, Set[Int], Set[Int], List[Int], Predef.Map[Int, Set[Int]])
  // list of (extends - -1 for trait, implementing, overrides, seals)
  type Output = List[(Int, List[Int], List[Int], List[Int])]
  type EnumType = Depend[Input, Output]

    import e.Enum

//  def fromSizeToPair(s: Int) = {
//    val actual = s - 1
//    (actual/5+1, actual%5)
//  }
  
  // measure for 2 methods
  def fromSizeToPair(s: Int) = (s, 2)

  val defMap = Predef.Map(-1 -> Set[Int](), 0 -> Set[Int]())

  def measureCode(tdEnum: EnumType) = {
    { (size: Int) =>
      val (cSize, mSize) = fromSizeToPair(size)
        val enum = tdEnum.getEnum((cSize, 1, Set(), Set(), 1 to mSize toList, defMap))

        for (ind <- 0 until enum.size) enum(ind)
    }
  }

  def warmUp(tdEnum: EnumType, maxSize: Int) {
//    for (size <- 1 to maxSize) {
      val (cSize, mSize) = fromSizeToPair(maxSize)
//        val enum = tdEnum.getEnum((cSize, 1, Set(), Set(), 1 to mSize toList, defMap))
//
//        for (ind <- 0 until enum.size) yield enum(ind)
      for (c <- 1 to cSize; m <- 0 to mSize) {
        val enum = tdEnum.getEnum((c, 1, Set(), Set(), 1 to m toList, defMap))

        for (ind <- 0 until enum.size) yield enum(ind)
      }
//    }
  }

  val subListChooser: DependFinite[(Int, List[Int]), List[Int]] = Depend.memoizedFin(
    (self: DependFinite[(Int, List[Int]), List[Int]], pair: (Int, List[Int])) => {
      val (size, range) = pair

      if (size <= 0) e.Singleton(Nil): Finite[List[Int]]
      else if (size == 1) e.Enum(range map { List(_) }): Finite[List[Int]]
      else if (size <= range.size) {
        val temp = self.getEnum((size - 1, range.tail))
        val kept = Map(temp, { range.head :: (_: List[Int]) })
        val leftOut = self.getEnum((size, range.tail))

        val allNodes = e.Concat(kept, leftOut)
        allNodes: Finite[List[Int]]
      } else e.Empty: Finite[List[Int]]
    })

  // given a list, pick n methods to override
  def overrid(implicit overridableMethods: List[Int]): DependFinite[Int, List[Int]] =
    Depend.fin((nMethods: Int) => subListChooser((nMethods, overridableMethods)): Finite[List[Int]])

  // given n and list, pick n methods to seal
  def seal: DependFinite[(Int, List[Int]), List[Int]] = Depend.fin((p: (Int, List[Int])) => {
    val (nMethods, overrides) = p
    subListChooser((nMethods, overrides)): Finite[List[Int]]
  })

  // pick nMethods to override and seal
  def overrideAndSeal(implicit overridableMethods: List[Int]): DependFinite[Int, ((Int, List[Int]), List[Int])] =
    Depend.fin((nMethods: Int) =>
      e.dependent.Chain[(Int, List[Int]), List[Int]](
        e.Product(e.Enum(0 to nMethods), overrid(overridableMethods)(nMethods)): Finite[(Int, List[Int])], // to seal ** overrides
        seal: DependFinite[(Int, List[Int]), List[Int]]))

  // pick a combination of override and seal
  def allOverrideAndSeal(allMethods: List[Int], sealedMap: Predef.Map[Int, Set[Int]]) =
    // ( #overrides, ((#seals, overrides), seals) ) 
    Depend.fin((p: (List[Int], Int)) => {
      val (implements, extend) = p

      val overridableMethods: List[Int] = allMethods.diff(
        ((extend :: implements).flatMap { sealedMap(_) }): List[Int])

      e.dependent.Chain(
        e.Enum(0 to overridableMethods.size): Finite[Int],
        overrideAndSeal(overridableMethods): DependFinite[Int, ((Int, List[Int]), List[Int])])
    })

  // pick which to implement
  def implements_(implicit interfaces: Set[Int]) =
    Map(e.dependent.Chain(
      // sizes
      Map(e.Enum(0 to interfaces.size): Finite[Int], { (_: Int, interfaces.toList) }),
      subListChooser), { (_: (_, List[Int]))._2 })

  // pick which to extend
  def extends_(implicit classes: Set[Int]) =
    // -1 interface, 0 class that does not extend anything, 1 to #classes which to extend
    e.Enum((classes + (-1) + (0)).toArray): Finite[Int]

  def makeAll(size: Int, classes: Set[Int], interfaces: Set[Int],
    overridableMethods: List[Int], map: Predef.Map[Int, Set[Int]]): Finite[((List[Int], Int), (Int, ((Int, List[Int]), List[Int])))] =
    e.dependent.Chain(
      e.Product(implements_(interfaces), extends_(classes)): Finite[(List[Int], Int)],
      allOverrideAndSeal(overridableMethods, map))

  def makeList(p: ((List[Int], Int), (Int, ((Int, List[Int]), List[Int])))) = {
    val ((impl, ext), (_, ((_, overriden), sealed_))) = p
    (ext, impl, overriden, sealed_) :: Nil
  }

  def constructEnumerator(implicit ms: MemoizationScope) = {

    Depend.fin(
      (self: EnumType, par: Input) => {
        // list sorted descendingly
        implicit val (size, myId, classes, interfaces, overridableMethods, sealedMap) = par

        //          if (size <= 0) e.Singleton(Nil): Finite[Output]
        //          else 
        if (size == 1) {
          Map(makeAll(size, classes, interfaces, overridableMethods, sealedMap), makeList): Finite[Output]
        } else {

          val rest: Depend[((List[Int], Int), (Int, ((Int, List[Int]), List[Int]))), Output] =
            InMap(self, { (par: ((List[Int], Int), (Int, ((Int, List[Int]), List[Int])))) =>
              val lastAdded = par
              val ((impl, ext), (_, ((_, overriden), sealed_))) = lastAdded

              val newClasses = if (ext >= 0) classes + myId else classes
              val newInterfaces = if (ext < 0) interfaces + myId else interfaces
              //                val newMethods = overridableMethods.diff(sealed_)

              // collect all sealed from parents
              val allParents =
                if (ext > 0) ext :: impl else impl
              val parentsSealed =
                (Set[Int]() /: allParents) { case (res, parent) => res union sealedMap(parent) }
              val newMap = sealedMap + (myId -> (parentsSealed union sealed_.toSet))

              (size - 1, myId + 1, newClasses, newInterfaces, overridableMethods, newMap)
            })

          e.dependent.Chain[((List[Int], Int), (Int, ((Int, List[Int]), List[Int]))), Output, Output](
            makeAll(size, classes, interfaces, overridableMethods, sealedMap): Enum[((List[Int], Int), (Int, ((Int, List[Int]), List[Int])))],
            rest: Depend[((List[Int], Int), (Int, ((Int, List[Int]), List[Int]))), Output],
            (r: ((List[Int], Int), (Int, ((Int, List[Int]), List[Int]))), o: Output) => { makeList(r) ::: o }): Finite[Output]

        }
      })
  }

}
