package insynth.engine

import insynth.structures.{ SuccinctType => Type, TSet }
import insynth.load.Declaration
import insynth.engine.scheduler.Scheduler

abstract class EnvironmentBuilder {
  
  private var taMap = Map.empty[Type, TypeAssignment]

//  def addDeclaration(name: String, scalaType: DomainType) {
//    addDeclaration(new Declaration(name, TypeTransformer(scalaType), scalaType))
//  }
  
  def addDeclarations(decls: Seq[Declaration]){
    decls.foreach(addDeclaration)
  }
  
  def addDeclaration(decl:Declaration) {
    val tpe = decl.getType

    taMap.get(tpe) match {
      case Some(ta) => 
        ta.addDeclaration(decl)
      case None => {
        val ta = new TypeAssignment(decl.getType)
        ta.addDeclaration(decl)
        getEnv.addTypeAssignment(ta)
        taMap += (tpe -> ta)
      }
    }
  }
  
  protected def getEnv:Environment

  def produceEnvirionment:Environment
  
  final private def copy(newEnvironmentBuilder: EnvironmentBuilder) {
    newEnvironmentBuilder.taMap = Map.empty
    newEnvironmentBuilder.taMap ++= taMap
  }
  
  override protected def clone = {
    val newEnvironmentBuilder = super.clone.asInstanceOf[EnvironmentBuilder]
    copy(newEnvironmentBuilder)
    newEnvironmentBuilder
  }
  
}

class InitialEnvironmentBuilder extends EnvironmentBuilder with Cloneable {
  
  def this(initialDecls: List[Declaration]) = {
    this()
    addDeclarations(initialDecls)
  }
  
  // this needs to be var (yes, cloning is very bad approach)
  private var env = new InitialEnvironment() 

  override protected def getEnv = env
  
  override def produceEnvirionment = env
  
  def getAllDeclarations = env.getAllDeclarations
    
  private final def copy(newInitialEnvironmentBuilder: InitialEnvironmentBuilder) {
    newInitialEnvironmentBuilder.env = env.clone.asInstanceOf[InitialEnvironment]
  }
  
  override def clone = {
    val newEnvironmentBuilder = super.clone.asInstanceOf[InitialEnvironmentBuilder]
    copy(newEnvironmentBuilder)
    newEnvironmentBuilder
  }
  
}

class DeltaEnvironmentBuilder(parent: Environment) extends EnvironmentBuilder with Cloneable {
  
  private val env = new DeltaEnvironment(parent)
  
  def create(set: TSet) {
    set.content.foreach{
      tpe => //this.addDeclaration(DeclarationFactory.makeAbstractDeclaration(tpe))
      	throw new RuntimeException("Fix this")
    }
  }
  
  override protected def getEnv = env 
  
  override def produceEnvirionment = if (!env.getTypeSet.equals(parent.getTypeSet)) env else parent
  
  override def clone = {
    super.clone.asInstanceOf[DeltaEnvironmentBuilder]
  }
    
}