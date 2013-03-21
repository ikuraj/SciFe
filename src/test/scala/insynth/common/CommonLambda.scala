package insynth.common

import insynth.reconstruction.stream._

import insynth.testdomain.{ TestQueryBuilder => QueryBuilder }

object CommonLambda {

  import CommonDeclarations._
  import CommonProofTrees._
  import CommonDomainTypes._
  
  val st = CommonSuccinctTypes
      
  val booleanIdentifier = Identifier(typeBoolean, booleanDeclaration)
  
  def constructBooleanToIntIntermediateLambda = {
    val query = new QueryBuilder(st.typeInt)

    val functionApplication =
      Application(
        functionBoolToIntType,
        List(
          Identifier(functionBoolToIntType, functionBoolToIntDeclaration),
          booleanIdentifier))

    val intermediateTree =
      Application(
        query.domainType,
        List(
          Identifier(query.domainType, query.getQuery.getDeclaration),
          functionApplication))

    List(intermediateTree)
  }
  
  def constructIntToIntIntermediateFirstLambda(x: Int) = {
    val query = new QueryBuilder(st.typeInt)

    val functionApplication =
      Application(
        functionIntToIntType,
        List(
          Identifier(functionIntToIntType, functionIntToIntDeclaration),
          Identifier(typeInt, intDeclaration)))

    val (_, listOfApplication) =
      (((Identifier(typeInt, intDeclaration), Nil): (Node, List[Node])) /: (1 to x)) {
      	case ((nextArg, list), _) =>
		      val app =	Application(
		        functionIntToIntType,
		        List(Identifier(functionIntToIntType, functionIntToIntDeclaration),
		          nextArg))
		          
          (app, list :+ app)
    	}
    
    for (app <- listOfApplication) yield 
    	Application(
        query.domainType,
        List(
          Identifier(query.domainType, query.getQuery.getDeclaration),
          app))    
  }    
  
  def constructIntAndBoolToIntIntermediateLambda(x: Int) = {
    val query = new QueryBuilder(st.typeInt)

    val functionApplicationBoolean =
      Application(
        functionBoolToIntType,
        List(
          Identifier(functionBoolToIntType, functionBoolToIntDeclaration),
          booleanIdentifier))
          
    val functionApplication =
      Application(
        functionIntToIntType,
        List(
          Identifier(functionIntToIntType, functionIntToIntDeclaration),
          Identifier(typeInt, intDeclaration)))

    val (listOfApplication, _) =
      (((Nil, List(Identifier(typeInt, intDeclaration), functionApplicationBoolean)): (List[Node], List[Node])) /: (1 to x)) {
      	case ((list, args), _) =>
		      val listAddition =
		        for (arg <- args) yield
			        Application(functionIntToIntType,
		        		List(Identifier(functionIntToIntType, functionIntToIntDeclaration), arg))
		          
          (list ++ listAddition, listAddition)
    	}
    
    for (app <- listOfApplication) yield 
    	Application(
        query.domainType,
        List(
          Identifier(query.domainType, query.getQuery.getDeclaration),
          app))    
  }  
  
  def constructThreeParFunctionIntermediateLambda(x: Int) = {
    val query = new QueryBuilder(st.typeInt)

    val listOfApplication =
      ((List(Identifier(typeInt, intDeclaration), Identifier(typeInt, intDeclaration)): List[Node]) /: (1 to x)) {
      	case (list, _) =>
		      val listAddition =
		        (for (arg <- list.combinations(2)) yield
			        Application(
					      threeParFunctionType,
					      List(
					        Identifier(threeParFunctionType, threeParFunctionDeclaration),
					        arg(0),
					        arg(1),
					        booleanIdentifier         
					      )
					    )) ++		      	
		        (for (arg <- list.combinations(2)) yield
			        Application(
					      threeParFunctionType,
					      List(
					        Identifier(threeParFunctionType, threeParFunctionDeclaration),
					        arg(1),
					        arg(0),
					        booleanIdentifier         
					      )
					    ))  ++		      	
		        (for (arg <- list) yield
			        Application(
					      threeParFunctionType,
					      List(
					        Identifier(threeParFunctionType, threeParFunctionDeclaration),
					        arg, arg,
					        booleanIdentifier         
					      )
					    ))				    
		          
          (list ++ listAddition).distinct
    	}
    
    for (app <- listOfApplication.distinct) yield 
    	Application(
        query.domainType,
        List(
          Identifier(query.domainType, query.getQuery.getDeclaration),
          app))
  }
  
  val boolInv = Application(functionBoolToIntType, List(
    Identifier(functionBoolToIntType, functionBoolToIntDeclaration), booleanIdentifier))
  val inv1WithBoolInv = Application(functionIntToIntType, List(
    Identifier(functionIntToIntType, functionIntToIntDeclaration), boolInv))
  val inv1WithInt = Application(functionIntToIntType, List(
    Identifier(functionIntToIntType, functionIntToIntDeclaration), Identifier(typeInt, intDeclaration)))
  val inv2WithInt = Application(functionIntToIntType, List(
    Identifier(functionIntToIntType, functionIntToIntDeclaration), inv1WithInt))
  val inv3WithInt = Application(functionIntToIntType, List(
    Identifier(functionIntToIntType, functionIntToIntDeclaration), inv2WithInt))
  val inv2WithBoolInv = Application(functionIntToIntType, List(
    Identifier(functionIntToIntType, functionIntToIntDeclaration), inv1WithBoolInv))
  val inv3WithBoolInv = Application(functionIntToIntType, List(
    Identifier(functionIntToIntType, functionIntToIntDeclaration), inv2WithBoolInv))
  
  // TODO do if we need abstraction (high-order functions)
//  def constructFunctionIntToIntIntermediateLambda = {
//    val query = new QueryBuilder(FunctionType(List(Int32Type), Int32Type))
//
//    val functionApplicationBoolean =
//      Application(
//        functionBoolToIntType,
//        List(
//          Set(Identifier(functionIntToIntType, functionBoolToIntDeclaration)),
//          Set(booleanIdentifier)))
//          
//    val functionApplication =
//      Application(
//        functionIntToIntType,
//        List(
//          Set(Identifier(functionIntToIntType, functionIntToIntDeclaration)),
//          Set(Variable(Int32Type, "freshInt"), functionApplicationBoolean)))
//
//    functionApplication.recursiveParams = List(Set(functionApplication))
//	
//		val abstraction = Abstraction(functionIntToIntType,
//	    List(Variable(Int32Type, "var_1")), Set(functionApplicationBoolean, functionApplication))
//
//    val intermediateTree =
//      Application(
//        query.leonType,
//        List(
//          Set(Identifier(query.leonType, query.getQuery.getDeclaration)),
//          Set(abstraction)))
//
//    intermediateTree
//  }

}