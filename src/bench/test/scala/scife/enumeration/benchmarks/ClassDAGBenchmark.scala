//package scife
//package enumeration
//package benchmarks
//
//import dependent._
//import scife.{ enumeration => e }
//import memoization._
//
//import scife.util._
//import scife.util.logging._
//import structures.BSTrees._
//
//import org.scalatest._
//import org.scalameter.api._
//
//import scala.language.postfixOps
//import scala.language.existentials
//
//class ClassDAGBenchmark
//  extends StructuresBenchmark[Depend[
//    //(Int, List[Int], List[Int], List[Int]), List[(List[Int], List[Int], List[Int])]
//    (Int, Int, Int, List[Int]), List[(Int, List[Int], List[Int], List[Int])]
//  ]]
//  with java.io.Serializable with HasLogger {
//
//  // (size, #class, #interface, #overridableMethods)
////  type Input = (Int, List[Int], List[Int], List[Int])
//  type Input = (Int, Int, Int, List[Int])
//  // list of (extends - -1 for trait, implementing, overrides, seals)
//  type Output = List[(Int, List[Int], List[Int], List[Int])]
//  type EnumType = Depend[Input, Output]
//
//  //  import e.Enum
//
//  override def name = "ClassDAG"
//
//  val maxSize = 5
//
//
//
//  def measureCode(using: super.Using[Int], tdEnum: EnumType) = {
//    using in { (size: Int) =>
//      for (c <- 1 to size; m <- 0 to size) {
//        val enum = tdEnum.getEnum((c, 0, 0, 1 to m toList))
//
//        for ( ind <- 0 until enum.size ) yield enum(ind)
//      }
//    }
//  }
//
//  def warmUp(tdEnum: EnumType) {
//    for (size <- 1 to maxSize) {
//      for (c <- 1 to size; m <- 0 to size) {
//        val enum = tdEnum.getEnum((c, 0, 0, 1 to m toList))
//
//        for ( ind <- 0 until enum.size ) yield enum(ind)
//      }
//    }
//  }
//
//  def getRange(m: Int) = m to 0 by -1
//
////  def constructEnumerator(implicit ms: MemoizationScope) = {
////
////    Depend.memoized(
////      (self: EnumType, par: Input) => {
////      // list sorted descendingly
////      val (size, classes, interfaces, overridableMethods) = par
////
////      // the number of overrides and seals
////      val overrides = Enum(0 to overridableMethods)
////      val seals = Depend( (v: Int) => Enum(0 to v) )
////
////      val overridesAndSeals = e.dependent.Chain(
////        overrides, seals
////      )
////
////      val isClass = Enum( Array(true, false) )
////      val implements = Enum( 0 to interfaces )
////
////      if (size <= 0) e.Singleton(Leaf)
////      else if (size == 1) {
////        e.Product(
////          e.Concat(
////            e.Product(e.Enum(0 until classes.size), implements_),
////            implements_ map { (-1, _) }
////          ),
////          overrideAndSeal(overridableMethods.size)
////        ) map { case ( (ext, impl), ((_, sealed_), overriden)) =>
////          if (ext > -1) List(ext :: impl, overriden, sealed_)
////          else List (impl, overriden, sealed_)
////          }
////      }
////      else if (!range.isEmpty) {
////
////        whichDefine = setChooser
////        whichSubclass = setChosoer
////        which Finalize = setChooser
////
////      }
////    })
////  }
//
//  val isClass = Depend.memoized(
//    (set: Set[Boolean]) => { e.WrapArray(set.toArray) })
//
//  def constructEnumerator(implicit ms: MemoizationScope) = {
//
//    val subListChooser: Depend[(Int, List[Int]), List[Int]] = Depend.memoized(
//      (self: Depend[(Int, List[Int]), List[Int]], pair: (Int, List[Int])) => {
//        val (size, range) = pair
//
//        if (size <= 0) e.Empty: Enum[List[Int]]
//        else if (size == 1) e.Enum(range map {List(_)}): Enum[List[Int]]
//        else if (size <= range.size) {
//          val kept = self.getEnum( (size - 1, range.tail) ) map { range.head :: _ }
//          val leftOut = self.getEnum( (size, range.tail) )
//
//          val allNodes = e.Concat(kept, leftOut)
//          allNodes: Enum[List[Int]]
//        } else e.Empty: Enum[List[Int]]
//      })
//
//    Depend.memoized(
//      (self: EnumType, par: Input) => {
//      // list sorted descendingly
//      val (size, classes, interfaces, overridableMethods) = par
//
//      // pick nMethods to override
//      def overrid = Depend( (nMethods: Int) => subListChooser( (nMethods, overridableMethods) ) )
//
//      // pick nMethods to seal
//      def seal = Depend( ( p: (Int, List[Int]) ) => {
//        val (nMethods, overrides) = p
//        subListChooser( (nMethods, overrides) )
//      } )
//
//      // pick nMethods to override and seal
//      def overrideAndSeal = Depend( (nMethods: Int) =>
//        e.dependent.Chain(
//          e.Product(e.Enum( 0 to nMethods), overrid(nMethods)), // to seal ** overrides
//          seal
//        )
//      )
//
//      // pick a combination of override and seal
//      def allOverrideAndSeal
//      // ( #overrides, ((#seals, overrides), seals) )
//      : Finite[(Int, ((Int, List[Int]), List[Int]))] = e.dependent.Chain(
//        e.Enum( 0 to overridableMethods.size ),
//        overrideAndSeal
//      )
//
//      // pick which to implement
//      def implements_ =
//        Map(e.dependent.Chain(
//          Map(e.Enum(0 to interfaces), { (_: Int, 1 to interfaces toList)  }), subListChooser
//        ), { (_: (_, List[Int]))._2 })
//
//      // pick which to extend
//      def extends_ =
//        // -1 interface, 0 class that does not extend anything, 1 to #classes which to extend
//        e.Enum(0 to classes)
//
//      def makeAll: Finite[((List[Int], Int), (Int, ((Int, List[Int]), List[Int])))] =
//        e.Product( e.Product( implements_, extends_ ): Finite[(List[Int], Int)], allOverrideAndSeal)
//
////      if (size <= 0) e.Singleton(Nil): Finite[Output]
////      else
//      if (size == 1) {
//        Map( makeAll,
//        { p: ((List[Int], Int), (Int, ((Int, List[Int]), List[Int]))) =>
//          val ( (impl, ext), (_, ((_, overriden), sealed_) ) ) = p
//
//          (ext, impl, overriden, sealed_) :: Nil
//        } ): Finite[Output]
//      }
//      else {
//
//        val rest: Depend[((List[Int], Int), (Int, ((Int, List[Int]), List[Int]))), Output] =
//          InMap(self, { (par: ((List[Int], Int), (Int, ((Int, List[Int]), List[Int])))) =>
//            val lastAdded = par
//            val ((impl, ext), (_, ((_, overriden), sealed_) )) = lastAdded
//
//            val newClasses = if (ext >= 0) classes + 1 else classes
//            val newInterfaces = if (ext < 0) interfaces + 1 else interfaces
//            val newMethods = overridableMethods.diff(sealed_)
//
//            (size - 1, newClasses, newInterfaces, newMethods)
//          })
//
//        e.dependent.Chain[((List[Int], Int), (Int, ((Int, List[Int]), List[Int]))), Output, Output] (
//          makeAll: Enum[((List[Int], Int), (Int, ((Int, List[Int]), List[Int])))],
//          rest: Depend[((List[Int], Int), (Int, ((Int, List[Int]), List[Int]))), Output],
//          (r: ((List[Int], Int), (Int, ((Int, List[Int]), List[Int]))), o: Output ) => { o }
//        ): Enum[Output]
//
//      }
//    })
//  }
//
//}
