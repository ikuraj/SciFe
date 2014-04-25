package insynth.enumeration
package testcases

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._

import dependent._
import insynth.{ enumeration => e }
import memoization._

import insynth.util._
import insynth.util.logging._
import Structures.BSTrees._

import org.scalatest._
import org.scalameter.api._

import scala.language.postfixOps
import scala.language.existentials

class ClassDAGTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
	HasLogger with ProfileLogger { 
  import Checks._
  import Structures._
  import BSTrees._
  import Util._
  
  // (size, #class, #interface, #overridableMethods)
  type Input = (Int, Int, Int, List[Int])
  // list of (extends - -1 for trait, implementing, overrides, seals)
  type Output = List[(Int, List[Int], List[Int], List[Int])]
  type EnumType = Depend[Input, Output]

  test("enumeration") {
    val checkerHelper = new CheckerHelper[Output]
    import checkerHelper._
    
    def rangeList(m: Int) = m to 0 by -1 toArray
    val enum = constructEnumerator

    withLazyClue("Elements are: " + clue) {
        
      // for size 0 we do not support
//      for (m <- 0 to 5) {
//        res = enum.getEnum((0, 0, 0, 1 to m toList))
//        res shouldBe a [Singleton[_]]
//        res.size should be (0)
//      }
      
      
      for (c <- 1 to 5; m <- 1 to 5) {
      res = enum.getEnum((c, m, 0, Nil))
      // class or interface
        println(res.size)
      }

//      // (size, #class, #interface, #overridableMethods)
//      res = enum.getEnum((1, 0, 0, Nil))
//      res shouldBe a [Map[_, _]]
//      // class or interface
//      res.size should be (2)
//      
//      res = enum.getEnum((1, 1, 0, Nil))
//      res shouldBe a [Map[_, _]]
//      // class or interface
//      res.size should be (3)
//      
//      res = enum.getEnum((1, 0, 1, Nil))
//      res shouldBe a [Map[_, _]]
//      // class or interface
//      res.size should be (4)
//      
//      res = enum.getEnum((1, 1, 0, List(1)))
//      // class or interface
//      res.size should be (9)
//      
//      res = enum.getEnum((1, 1, 1, Nil))
//      res shouldBe a [Map[_, _]]
//      // class or interface
//      res.size should be (6)
//      
//      res = enum.getEnum((2, 0, 0, Nil))
//      res shouldBe a [ChainFiniteCombine[_, _, _]]
//      // class or interface
//      res.size should be (
//        2 // first: 1, 0, 0
//        + 2 // second
//        + 1 // can extend a class, in one case
//        + 2 // can implement an interface, in both cases
//      )
//      
//      res = enum.getEnum((1, 0, 0, List(1)))
//      res.size should be (6)
//      
//      res = enum.getEnum((1, 0, 0, List(1, 2)))
//      res.distinct.size should be (res.size)
////      val resSet = res.map( _.toSet )
////      resSet.distinct.size should be (resSet.size)
//      res.size should be (18)
//      
//      res = enum.getEnum((2, 0, 0, List(1)))
//      res.distinct.size should be (res.size)
////      val resSet = res.map( _.toSet )
////      resSet.distinct.size should be (resSet.size)
//      res.size should be (49)
//      
//      res = enum.getEnum((3, 0, 0, Nil))
//      res shouldBe a [ChainFiniteCombine[_, _, _]]
//      // class or interface
//      res.size should be ( 35 )
//
//	    for (m <- 2 to 10) {
//      	res = enum.getEnum( (1, rangeList(m: Int)) )
//      	res shouldBe a [WrapArray[_]]
//      	res.size should be (rangeList(m).size)
//	    }

//	    	for(s <- 1 to 10; m <- 1 to s) {
//	        addMessage = "m=%d and s=%d".format(m, s)
//	        res = enum.getEnum( (m, rangeList(m: Int)) )
//
//	        elements.forall( heapEqualProperty(_) ) should be (true)
//    		}
//	    	
//      addMessage = ""
//
//    	res = enum.getEnum( (3, rangeList(3)) )
//      elements.size should be (elements.distinct.size)
//    	res = enum.getEnum( (3, rangeList(3)) )
//      elements.size should be (elements.distinct.size)
//    	res.size should be (30)
//    	res = enum.getEnum( (4, rangeList(4)) )
//      elements.size should be (elements.distinct.size)
//    	res.size should be (30)
//    	res = enum.getEnum( (7, rangeList(7)) )
//      elements.size should be (elements.distinct.size)
//    	res.size should be (73644)
    }
  }
  
  test("subListChooser") {
    val checkerHelper = new CheckerHelper[List[Int]]
    import checkerHelper._
    
    def rangeList(m: Int) = m to 0 by -1 toArray
    val enum = constructEnumerator

    withLazyClue("Elements are: " + clue) {
      for(s <- 1 to 5; m <- 1 to s) {
        addMessage = "m=%d and s=%d".format(m, s)
        res = subListChooser.getEnum( (m, 1 to s toList) )
        val listCombinations: List[List[Int]] =
          ((1 to s toList) combinations m) toList
  
        res.size should be (listCombinations.size)
        elements should contain theSameElementsAs (listCombinations)
      }
    }
  }

  val subListChooser: DependFinite[(Int, List[Int]), List[Int]] = Depend.memoizedFin(
    (self: DependFinite[(Int, List[Int]), List[Int]], pair: (Int, List[Int])) => {
      val (size, range) = pair

      if (size <= 0) e.Singleton(Nil): Finite[List[Int]]
      else if (size == 1) e.Enum(range map {List(_)}): Finite[List[Int]]
      else if (size <= range.size) {
        val temp = self.getEnum( (size - 1, range.tail) )
        val kept = Map( temp , { range.head :: (_: List[Int]) })
        val leftOut = self.getEnum( (size, range.tail) )
        
        val allNodes = e.Concat(kept, leftOut)
        allNodes: Finite[List[Int]]
      } else e.Empty: Finite[List[Int]]
    })
      
  // pick nMethods to override
  def overrid(implicit overridableMethods: List[Int]): DependFinite[Int, List[Int]] =
    Depend.fin( (nMethods: Int) => subListChooser( (nMethods, overridableMethods) ): Finite[List[Int]] )
  
  // pick nMethods to seal
  def seal: DependFinite[(Int, List[Int]), List[Int]] = Depend.fin( ( p: (Int, List[Int]) ) => {
    val (nMethods, overrides) = p
    subListChooser( (nMethods, overrides) ): Finite[List[Int]]
  } )
  
  // pick nMethods to override and seal
  def overrideAndSeal(implicit overridableMethods: List[Int]): DependFinite[Int, ( (Int, List[Int]), List[Int] )] =
    Depend.fin( (nMethods: Int) =>
      e.dependent.Chain[(Int, List[Int]), List[Int]](
        e.Product(e.Enum( 0 to nMethods), overrid(overridableMethods)(nMethods)): Finite[(Int, List[Int])], // to seal ** overrides
        seal: DependFinite[(Int, List[Int]), List[Int]]
      )
    )
    
  // pick a combination of override and seal
  def allOverrideAndSeal(implicit overridableMethods: List[Int])
  // ( #overrides, ((#seals, overrides), seals) )
  : Finite[(Int, ((Int, List[Int]), List[Int]))] = e.dependent.Chain(
    e.Enum( 0 to overridableMethods.size ): Finite[Int],
    overrideAndSeal: DependFinite[Int, ((Int, List[Int]), List[Int])]
  )
  
  test("override and seals") {
    e.Enum( 0 to 1 ).size should be (2)

    for(s <- 1 to 5) {
      val list = 1 to s toList;
      withClue( "list is: " + list ) {
      
        for (i1 <- 0 to s; comb1 <- list.combinations(i1))
//            overrid(list)(i1).toList should contain ( (i1, comb1) )
          overrid(list)(i1).toList should contain ( comb1 )

        val res = allOverrideAndSeal(list)
        val resList: List[(Int, ((Int, List[Int]), List[Int]))] = res.toList
        
        for (i1 <- 0 to s; comb1 <- list.combinations(i1);
            i2 <- 0 to i1; comb2 <- comb1.combinations(i2)) {
          resList should contain ( (i1, ((i2, comb1), comb2)): (Int, ((Int, List[Int]), List[Int])) )
        }
      
      }
      
    }
  }
    
  // pick which to implement
  def implements_(implicit interfaces: Int)=
    Map(e.dependent.Chain(
      Map(e.Enum(0 to interfaces): Finite[Int], { (_: Int, 1 to interfaces toList)  }), subListChooser
    ), { (_: (_, List[Int]))._2 })
    
  // pick which to extend
  def extends_(implicit classes: Int) =
    // -1 interface, 0 class that does not extend anything, 1 to #classes which to extend
    e.Enum(-1 to classes)
  
  test("extends and implements") {
    for(c <- 0 to 10; i <- 0 to (10 - c)) {
      withClue( "(c, i):" + (c, i) ) {
      
        extends_(c).size should be (c + 2)
        
        implements_(i).size should be (math.pow(2, i).toInt)
        
      }
    }
  }
    
  def makeAll(size: Int, classes: Int, interfaces: Int, overridableMethods: List[Int]):
  Finite[((List[Int], Int), (Int, ((Int, List[Int]), List[Int])))] =
    e.Product( e.Product( implements_(interfaces), extends_(classes) ): Finite[(List[Int], Int)],
      allOverrideAndSeal(overridableMethods))
      
      
  def makeList( p: ((List[Int], Int), (Int, ((Int, List[Int]), List[Int]))) ) = {
    val ( (impl, ext), (_, ((_, overriden), sealed_) ) ) = p

    (ext, impl, overriden, sealed_) :: Nil
  }

  def constructEnumerator(implicit ms: MemoizationScope = null) = {
    
    Depend.memoized(
      (self: EnumType, par: Input) => {
      // list sorted descendingly
      implicit val (size, classes, interfaces, overridableMethods) = par

//      if (size <= 0) e.Singleton(Nil): Finite[Output]
//      else 
      if (size == 1) {        
        Map( makeAll(size, classes, interfaces, overridableMethods), makeList ): Finite[Output]
      }
      else {

        val rest: Depend[((List[Int], Int), (Int, ((Int, List[Int]), List[Int]))), Output] =
          InMap(self, { (par: ((List[Int], Int), (Int, ((Int, List[Int]), List[Int])))) =>
            val lastAdded = par
            val ((impl, ext), (_, ((_, overriden), sealed_) )) = lastAdded
            
            val newClasses = if (ext >= 0) classes + 1 else classes
            val newInterfaces = if (ext < 0) interfaces + 1 else interfaces
            val newMethods = overridableMethods.diff(sealed_)
            
            (size - 1, newClasses, newInterfaces, newMethods)
          })
        
        e.dependent.Chain[((List[Int], Int), (Int, ((Int, List[Int]), List[Int]))), Output, Output] (
          makeAll(size, classes, interfaces, overridableMethods): Enum[((List[Int], Int), (Int, ((Int, List[Int]), List[Int])))],
          rest: Depend[((List[Int], Int), (Int, ((Int, List[Int]), List[Int]))), Output],
          (r: ((List[Int], Int), (Int, ((Int, List[Int]), List[Int]))), o: Output ) => { makeList(r) ::: o }
        ): Finite[Output]
        
      }
    })
  }
  
}
