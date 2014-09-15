package scife.enumeration.testcases

import org.scalatest._
import org.scalatest.prop._
import org.scalatest.matchers._

import scife.enumeration._
import dependent._
import scife.{ enumeration => e }
import memoization._

import scife.util._
import scife.util.logging._
import Structures.BSTrees._

import org.scalatest._
import org.scalameter.api._

import scala.language.postfixOps
import scala.language.existentials

// this testcase has only classes, not interfaces
class ClassDAGTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with
	logging.HasLogger with ProfileLogger { 
  import Checks._
  import Structures._
  import BSTrees._
  import Util._
  
  // (size, Id, #class, #overridableMethods, map(node->sealed))
  type Input = (Int, Int, Set[Int], List[Int], Predef.Map[Int, Set[Int]])
  // list of (extends - -1 for trait, overrides, seals)
  type Output = List[(Int, List[Int], List[Int])]
  type EnumType = Depend[Input, Output]
      
  val defMap = Predef.Map( -1 -> Set[Int](), 0 -> Set[Int]() )
  
  def toGraph(i: Input, o: Output) = {
    import scife.enumeration.testcases.classdag._
    
    val (size, myId, classes, overridableMethods, sealedMap) = i
    
    val graph = new DAG
    graph.setSize(size)
    graph.setMethodsNum(overridableMethods.size)
    
    val parentMap = scala.collection.mutable.Map[Int, Array[Boolean]]()
    for (i <- 1 to size)
      parentMap(i) = new Array(size)
    
    val nodes =
	    for ((myNode, ind_) <- o.zipWithIndex) yield {
	       val ind = ind_ + 1
	       val (ext, overrides, seals) = myNode
	       
	       val dagNode = new DAGNode
	       dagNode.numChildren = size
	       
	       dagNode.allBools = Array.fill(size + overridableMethods.size * 2)(false)
	       for (overrid <- overrides)
	         dagNode.allBools(size + overrid -1 ) = true
	       for (fin <- seals)
	         dagNode.allBools(size + overridableMethods.size + fin - 1) = true
	       
	       if (ext > 0) parentMap(ext)(ind-1) = true
	       
	       dagNode
	    }
    
    for ((node, nid) <- nodes.zipWithIndex) {
      for (cid <- 0 until size)
        node.allBools(cid) = parentMap(nid + 1)(cid)
    }
    
    import scala.collection.JavaConversions._
    
    graph.setNodes(nodes)
    
    graph
  }

  test("enumeration") {
    val checkerHelper = new CheckerHelper[Output]
    import checkerHelper._
    
    def rangeList(m: Int) = m to 0 by -1 toArray
    val enum = constructEnumerator

    // (size, id, #class, #overridableMethods, sealedMap)
    res = enum.getEnum((1, 1, Set(), Nil, defMap))
    res shouldBe a[Map[_, _]]
    res.size should be(1)

    res = enum.getEnum((1, 1, Set(1), Nil, defMap + (1 -> Set())))
    res shouldBe a[Map[_, _]]
    res.size should be(2)

    res = enum.getEnum((1, 1, Set(), List(1), defMap))
    res.size should be(3)

    res = enum.getEnum((1, 1, Set(1), List(1), defMap + (1 -> Set())))
    res.size should be(6)

    res = enum.getEnum((2, 1, Set(), List(1), defMap))
    res.size should be(16)

    res = enum.getEnum((1, 1, Set(), List(1, 2), defMap))
    res.size should be(9)

    res = enum.getEnum((2, 1, Set(), List(1, 2), defMap))
    res.size should be(130)

    for (c <- 1 to 3; m <- 0 to 2) {
      val input = (c, 1, Set[Int](), 1 to m toList, defMap)

      res = enum.getEnum(input)
      res.distinct.size should be(res.size)
      for (el <- res; g = toGraph(input, el))
        withClue(el + "\nGraph:\n" + g) {
          g.repOK() should be(true)
        }
    }

    {
		  val input = (3, 1, Set[Int](), List(1), defMap)
		  res = enum.getEnum( input )
		  info(res.map( toGraph(input, _) ).toList.mkString("\n"))
		  res.size should be(124)
    }

    for (c <- 1 to 3; m <- 0 to 2) {
      val input = (c, 1, Set[Int](), 1 to m toList, defMap)
      res = enum.getEnum(input)

      val message = "(c = %d, m = %d)".format(c, m)
      withClue(message) {
        info(message + res.size)
      }
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
      
  // given a list, pick n methods to override
  def overrid(implicit overridableMethods: List[Int]): DependFinite[Int, List[Int]] =
    Depend.fin( (nMethods: Int) => subListChooser( (nMethods, overridableMethods) ): Finite[List[Int]] )
  
  // given n and list, pick n methods to seal
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
  def allOverrideAndSeal(allMethods: List[Int], sealedMap: Predef.Map[Int, Set[Int]]) =    
	  Depend.fin( (p: Int) => {
	    	val extend = p

	    	val overridableMethods: List[Int] = allMethods.diff(
    	    sealedMap(extend).toList
  	    )
	      
			  e.dependent.Chain(
			    e.Enum( 0 to overridableMethods.size ): Finite[Int],
			    overrideAndSeal(overridableMethods): DependFinite[Int, ((Int, List[Int]), List[Int])]
			  )
		  }
	  )
	  
  test("override and seals") {
    e.Enum( 0 to 1 ).size should be (2)

    for(s <- 1 to 5) {
      val list = 1 to s toList;

      withClue( "list is: " + list ) {
      
        for (i1 <- 0 to s; comb1 <- list.combinations(i1))
//            overrid(list)(i1).toList should contain ( (i1, comb1) )
          overrid(list)(i1).toList should contain ( comb1 )
        
        val map = Predef.Map(1 -> Set[Int]())
        val res = allOverrideAndSeal(list, map)(1)
        val resList: List[(Int, ((Int, List[Int]), List[Int]))] = res.toList
        
        for (i1 <- 0 to s; comb1 <- list.combinations(i1);
            i2 <- 0 to i1; comb2 <- comb1.combinations(i2)) {
          resList should contain ( (i1, ((i2, comb1), comb2)): (Int, ((Int, List[Int]), List[Int])) )
        }
      
      }

    }
  }
    
  // pick which to extend
  def extends_(implicit classes: Set[Int]) =
    // -1 interface, 0 class that does not extend anything, 1 to #classes which to extend
    e.Enum((classes + (0)).toArray): Finite[Int]
  
  test("extends") {
    for(c <- 0 to 10; i <- 0 to (10 - c)) {
      withClue( "(c, i):" + (c, i) ) {
      
        extends_(-1 to c toSet).size should be (c + 2)
        
      }
    }
    
    extends_(Set()).size should be (1)
  }
    
  def makeAll(size: Int, classes: Set[Int], 
    overridableMethods: List[Int], map: Predef.Map[Int, Set[Int]]):
  Finite[(Int, (Int, ((Int, List[Int]), List[Int])))] =
    e.dependent.Chain(
      extends_(classes): Finite[Int],
      allOverrideAndSeal(overridableMethods, map)
    )
      
      
  def makeList( p: (Int, (Int, ((Int, List[Int]), List[Int]))) ): Output = {
    val ( ext, (_, ((_, overriden), sealed_) ) ) = p

    (ext, overriden, sealed_) :: Nil
  }

  def constructEnumerator(implicit ms: MemoizationScope = null) = {
    
    Depend.memoized(
      (self: EnumType, par: Input) => {
      // list sorted descendingly
      implicit val (size, myId, classes, overridableMethods, sealedMap) = par
      
//      if (size <= 0) e.Singleton(Nil): Finite[Output]
//      else 
      if (size == 1) {        
        Map( makeAll(size, classes, overridableMethods, sealedMap), makeList ): Finite[Output]
      }
      else {

        val rest: Depend[( Int, (Int, ((Int, List[Int]), List[Int]))), Output] =
          InMap(self, { (par: (Int, (Int, ((Int, List[Int]), List[Int])))) =>
            val lastAdded = par
            val (ext, (_, ((_, overriden), sealed_) )) = lastAdded
            
            val newClasses = if (ext >= 0) classes + myId else classes
            
            // collect all sealed from parents
            val allParents = ext :: Nil
            val parentsSealed = 
              ( Set[Int]() /: allParents ) { case (res, parent) => res union sealedMap(parent) }
            val newMap = sealedMap + ( myId -> (parentsSealed union sealed_.toSet) )
            
            (size - 1, myId + 1, newClasses, overridableMethods, newMap)
          })
        
        e.dependent.Chain (
          makeAll(size, classes, overridableMethods, sealedMap): Enum[( Int, (Int, ((Int, List[Int]), List[Int])))],
          rest: Depend[( Int, (Int, ((Int, List[Int]), List[Int]))), Output],
          (r: ( Int, (Int, ((Int, List[Int]), List[Int]))), o: Output ) => { makeList(r) ::: o }
        ): Finite[Output]
        
      }
    })
  }
  
}
