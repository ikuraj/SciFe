package insynth.engine

import insynth.structures.{ SuccinctType => Type }
import insynth.engine.scheduler.Scheduler

abstract class Environment {
  
  protected var clusters = Map.empty[Type, Cluster]
  
  def getCluster(tpe:Type) = {
    clusters.get(tpe) match {
      case Some(cluster) => cluster
      case None => null
    }
  }
  
  def addCluster(cluster:Cluster) {
    assert(cluster != null)    
    clusters += (cluster.getType -> cluster) 
  }
  
  def addTypeAssignment(ta:TypeAssignment)
  
  def getTypeAssignments(tpe:Type):List[TypeAssignment]

  def getTypeSet:Set[Type]
  
  def getClusters = clusters
  
  def getParent:Environment
  
  final private def copy(newEnv: Environment) {
    newEnv.clusters = Map.empty[Type, Cluster]
    newEnv.clusters ++= clusters
  }
  
  override protected def clone = {
    val newEnvironment = super.clone.asInstanceOf[Environment]
    copy(newEnvironment)
    newEnvironment
  }
  
}

class InitialEnvironment extends Environment with Cloneable {
    
  override def addTypeAssignment(ta:TypeAssignment) {
    val tpe = ta.getReturnType
    val cluster = this.getCluster(tpe)
    if(cluster != null){
      cluster.addTypeAssignment(ta)
    } else {
      val newCluster = new InitialCluster(tpe)
      newCluster.addTypeAssignment(ta)
      this.addCluster(newCluster)
    }
  }
  
  override def getTypeAssignments(tpe:Type) = {
    val cluster = this.getCluster(tpe)
    if (cluster != null) cluster.getTypeAssignments
    else List.empty[TypeAssignment]
  }
  
  override def getTypeSet = Set.empty[Type]
  
  override def getParent = null
  
  def getAllDeclarations = clusters.values.toList.map(cluster => cluster.getTypeAssignments.map(ta => ta.getDeclarations).flatten).flatten
  
  override def clone = {
    super.clone.asInstanceOf[InitialEnvironment]
  }
}

class DeltaEnvironment(parent: Environment) extends Environment with Cloneable {
  assert(parent != null)
  
  private var typeSet = parent.getTypeSet
    
  override def addTypeAssignment(ta:TypeAssignment) {
    val tpe = ta.getType
    if (!typeSet.contains(tpe)){
      typeSet += tpe
      
      val retType = ta.getReturnType
    
      //get a cluster from a child
      val cluster = this.getCluster(retType)
      if(cluster != null){
        cluster.addTypeAssignment(ta)
      } else {
      
        //get a cluster from a parent 
        val parentCluster = parent.getCluster(retType)
      
        //anyways create new cluster that may or may not point to the parent cluster
        val newCluster = if (parentCluster != null) new DeltaCluster(retType, parentCluster)
                                             else new InitialCluster(retType)

        //add new cluster
        this.addCluster(newCluster)
        newCluster.addTypeAssignment(ta)
      }
    }
  }

  override def getParent = parent
  
  override def getTypeSet = typeSet
  
  //TODO: Think about better design and solution
  override def getTypeAssignments(tpe:Type) = {
    var cluster = this.getCluster(tpe)
    var parent = this.getParent
    while(parent != null && cluster == null) {
      cluster = parent.getCluster(tpe)
      parent = parent.getParent
    }
      
    if (cluster != null) {
      cluster.getTypeAssignments
    } else List.empty[TypeAssignment]
  }
  
  final private def copy(newDeltaEnvironment: DeltaEnvironment) {
    newDeltaEnvironment.typeSet = Set.empty
    newDeltaEnvironment.typeSet ++= typeSet
  }
  
  override def clone = {
    val newDeltaEnvironment = super.clone.asInstanceOf[DeltaEnvironment]
    copy(newDeltaEnvironment)
    newDeltaEnvironment
  }
  
/* Old  
  override def getTypeAssignments(tpe:Type) = {
    val cluster = this.getCluster(tpe)
    if (cluster != null) {
      cluster.getTypeAssignments
    } else {
      val parentCluster = parent.getCluster(tpe)
      if (parentCluster != null) {
        parentCluster.getTypeAssignments
      } else List.empty[TypeAssignment]
    }
  }
*/  
}