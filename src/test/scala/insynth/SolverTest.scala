package insynth

import insynth._
import insynth.testdomain._
import insynth.common._

import org.scalatest._

class SolverTest extends FunSpec with GivenWhenThen with Matchers {    
  
  import CommonDeclarations._
  import CommonSuccinctTypes._
  import insynth.util.ProofTreeOperations._

  describe("Solver") {

    it("should be properly initialized") {
      
      val inputDeclarations = List(booleanDeclaration, functionBoolToIntDeclaration)
      val queryBuilder = TestQueryBuilder(typeInt)
      
      Given("a solver")
      val solver = new Solver(
        inputDeclarations,queryBuilder
    	)
      
      Then("queries should be appropriate")
      solver.allDeclarations.toSet should equal (inputDeclarations.toSet)
      solver.allDeclarations.size should equal (inputDeclarations.size)
      solver.query.getReturnType should equal (queryBuilder.getQuery.getReturnType)
      solver.query.getDeclaration should equal (queryBuilder.getQuery.getDeclaration)
      solver.currentBuilder should equal (solver.initialBuilder)
    }

    it("should compute valid a proof tree with bool->int, bool") {
      val inputDeclarations = List(booleanDeclaration, functionBoolToIntDeclaration)
      val queryBuilder = TestQueryBuilder(typeInt)
      
      Given("a solver")
      val solver = new Solver(
        inputDeclarations, queryBuilder
    	)
      
      Then("returned proof trees should be appropriate")
      val result = solver.getProofTree
      result should not be (null)
      result.getNodes should not be ('empty)
      withClue(
		    breadthFirstSearchPrint(result.getNodes.head)
	    ) {
      checkInhabitants(result,
        StringNode(functionBoolToIntDeclaration.getSimpleName, Set(
          StringNode(booleanDeclaration.getSimpleName)
        ))) should be (true)
      }
    }

    it("should compute valid a proof tree with bool->int, int, int->int, bool") {
      val inputDeclarations = List(booleanDeclaration, functionBoolToIntDeclaration,
        functionIntToIntDeclaration, intDeclaration)
      val queryBuilder = TestQueryBuilder(typeInt)
      
      Given("a solver")
      val solver = new Solver(
        inputDeclarations, queryBuilder
    	)
      
      Then("returned proof tree should contain f1(bool)")
      val result = solver.getProofTree
      result should not be (null)
      result.getNodes should not be ('empty)
      withClue(
		    breadthFirstSearchPrint(result.getNodes.head)
	    ) {
      checkInhabitants(result,
        StringNode(functionBoolToIntDeclaration.getSimpleName, Set(
          StringNode(booleanDeclaration.getSimpleName)
        ))) should be (true)
      }
            
      And("returned proof tree should contain f2(int), f2(f2(int), f2(f1(bool))")
      withClue(
		    breadthFirstSearchPrint(result.getNodes.head)
	    ) {
      checkInhabitants(result,
        StringNode(functionIntToIntDeclaration.getSimpleName, Set(
          StringNode(intDeclaration.getSimpleName),
          StringNode(functionIntToIntDeclaration.getSimpleName, Set(
	          StringNode(intDeclaration.getSimpleName)
	        )),
          StringNode(functionBoolToIntDeclaration.getSimpleName, Set(
	          StringNode(booleanDeclaration.getSimpleName)
	        ))
        ))) should be (true)
      }
    }

    it("should be sensitive to timeout value") {
      val inputDeclarations = List(booleanDeclaration, functionBoolToIntDeclaration,
        functionIntToIntDeclaration, intDeclaration)
      val queryBuilder = TestQueryBuilder(typeInt)
      
      Given("a solver with 0 timeout")
      val solver = new Solver(
        inputDeclarations, queryBuilder, 0
    	)
      
      Then("returned proof tree should be empty")
      val result = solver.getProofTree
      result should be (null)
    }

    ignore("Fix this ASAP!") { 
    it("should compute valid a proof tree if function has to be created") {
      val inputDeclarations = List(booleanDeclaration, unitDeclaration,
        functionFunsToFunDeclaration)
      val queryBuilder = TestQueryBuilder(typeInt)
      
      Given("a solver with 0 timeout")
      val solver = new Solver(
        inputDeclarations, queryBuilder
    	)
      
      Then("returned proof tree should be empty")
      val result = solver.getProofTree
      result should not be (null)
      result.getNodes should not be ('empty)
    }
 
    it("should compute valid a proof tree if function has function parameters") {
      val inputDeclarations = List(booleanDeclaration, functionBoolToIntDeclaration,
        functionFun1ToUnitDeclaration, intDeclaration, functionFunsToFunDeclaration)
      val queryBuilder = TestQueryBuilder(typeInt)
      
      Given("a solver with 0 timeout")
      val solver = new Solver(
        inputDeclarations, queryBuilder
    	)
      
      Then("returned proof tree should be empty")
      val result = solver.getProofTree
      result should not be (null)
      result.getNodes should not be ('empty)
      
      And("returned proof tree should contain (ff(f1, f2))(int)")
      withClue(
		    breadthFirstSearchPrint(result.getNodes.head)
	    ) {
      checkInhabitants(result,
        StringNode(functionFunsToFunDeclaration.getSimpleName, Set(
          StringNode(intDeclaration.getSimpleName),
          StringNode(functionBoolToIntDeclaration.getSimpleName),
          StringNode(functionFun1ToUnitDeclaration.getSimpleName)
        ))) should be (true)
      }
    }
    }
    
  }
  
}