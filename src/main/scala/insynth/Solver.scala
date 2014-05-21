package insynth

import insynth.load._
import insynth.engine._
import insynth.query._
import insynth.engine.scheduler.WeightScheduler
import insynth.structures.ContainerNode

import insynth.util.TimeOut
import insynth.util.logging.HasLogger
import insynth.util.format.TreePrinter

/**
 * Main class for the synthesis process invocation
 * @param program Leon program object that contains the hole
 * @param hole hole in the program on which the synthesis is called 
 */
class Solver(declarations: List[Declaration], queryBuilder: QueryBuilder, timeOut: Int = 500) extends HasLogger {
  
  def this(loader: Loader, queryBuilder: QueryBuilder) = this(loader.load, queryBuilder)
  
  fine("declarations given to InSynth constructor:\n" + declarations.mkString("\n"))
  
  // create a new builder
  val initialBuilder = new InitialEnvironmentBuilder(declarations)
           
  // store all declarations in a field
  val allDeclarations: List[Declaration] = initialBuilder.getAllDeclarations
    
  // construct query for the given synthesis instance   
  val query =  queryBuilder.getQuery
  
  // builder updated after each synthesis
  var currentBuilder = initialBuilder
  
  /**
   * @param builder instance used in the engine search
   * @return container node representing the proof tree
   */
  def getProofTree(builder: InitialEnvironmentBuilder): ContainerNode = {   
	  info("all declarations saved, size: " + allDeclarations.size)
	  finer("declarations seen:\n" + builder.getAllDeclarations.mkString("\n"))
	  
	  // create the engine
    val engine = new Engine(builder, query, new WeightScheduler(), TimeOut(timeOut))
    // measure time
    val time = System.currentTimeMillis      
	  // run the engine
    val solution = engine.run()

    if (solution != null) {
      info("Solution found in " + (System.currentTimeMillis - time) + " ms.")
      finest("Solution found: " + TreePrinter(solution, 3))
    } else 
      info("No solution found in " + (System.currentTimeMillis - time) + " ms")
    
    currentBuilder = builder
      
    solution
  }
  
  /**
   * @return container node representing the proof tree
   */
  def getProofTree: ContainerNode = {
    finer("initialBuilder.getAllDeclarations.size: " + initialBuilder.getAllDeclarations.size)
    finer("initialBuilder.clone.getAllDeclarations.size: " + initialBuilder.clone.getAllDeclarations.size)
    
    // call on new identical builder, with same declarations    
    getProofTree(initialBuilder.clone)    
  }

}