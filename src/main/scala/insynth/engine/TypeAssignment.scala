package insynth.engine

import insynth.structures.{ SuccinctType => Type }
import insynth.load.Declaration

class TypeAssignment(tpe:Type) {
  assert(tpe != null)
  
  private val paramTypes:List[Type] = Type.paramTypes(tpe)
  private val returnType:Type = Type.returnType(tpe)
  
  private var decls = List.empty[Declaration]
  
  private var workingRequests = List.empty[RequestContainer]
  private var newRequests = List.empty[Request]
  
  /*
   * Properties like:
   * (1) the distance form the main query 
   * (2) type properties (polymorphic, how many arguments, function) 
   * (3) how many new/processed queries do we have
   * (4) weight
   */
  private var properties:Properties = new Properties()

  def getProperties = properties
  
  def getType = tpe
  
  def addDeclaration(decl: Declaration) {
    decls = decl :: decls
    properties.setMinWeight(decl.getWeight)
  }
  
  def addRequest(request:Request) {
    newRequests = request :: newRequests
  }

  def processRequests(requests: Requests) {
    
    val containerRequests = newRequests.map(new RequestContainer(_, decls, paramTypes))
    
    containerRequests.foreach {
      _.sendRequests(requests)
    }
    
    newRequests = Nil
    workingRequests :::= containerRequests
  }
  
  def getDeclarations = decls
 
  def getReturnType = returnType
}