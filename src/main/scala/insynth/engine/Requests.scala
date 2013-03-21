package insynth.engine

import insynth.structures.{ SuccinctType => Type, _ }
import scheduler.ListenerHolder
import scheduler.Subject
import insynth.load.Declaration

class Requests extends ListenerHolder {
  
  private var clusters = Map.empty[Type, RequestCluster]
 
  def addSender(returnType:Type, env: Environment, sender:Sender){
    clusters.get(returnType) match {
      case None =>
        val cluster = new RequestCluster(returnType)
        
        cluster.registerAllListener(listeners)        
        
        cluster.addSender(env, sender)
        clusters += (returnType -> cluster)
      case Some(cluster) => 
        cluster.addSender(env, sender)
    }
  }
  
  def getCluster(returnType:Type) = clusters.get(returnType)
  
}

class RequestCluster(returnType:Type) extends Subject {
  assert(returnType != null)
  
  private var requests = Map.empty[Environment, Request]
  
  def addSender(env:Environment, sender:Sender){
    requests.get(env) match {
      case None =>
        val request = new Request(env)
        request.addSender(sender)
        requests += (env -> request)
        
        //putting request in tas queues 
        val tas = env.getTypeAssignments(returnType)
        
        //notify Listener
        this.notifyListeners(tas)
        
        tas.foreach(_.addRequest(request))
      case Some(request) => 
        request.addSender(sender)
    }
  }
  
  def getRequest(env:Environment) = requests.get(env)
}

/**
 * 'requestType' is the most important here. It is the type that we are searching for.
 * 'senderType' is that type of the parameter that initiated the request. It is also the same as the type of the cluster that holds requests.
 */
class Request(env:Environment) {
  assert(env != null)
  
  private var senders = Set.empty[Sender]
  private var answers:ContainerNode = null
  
  def addAnswer(answer:SimpleNode) {
    if (answers == null){
      answers = new ContainerNode(scala.collection.mutable.Set.empty)
      answers.addNode(answer)
      //Update senders
      updateSenders()
    } else {
      answers.addNode(answer)
    }
  }
  
  private def updateSenders() {   
    senders.foreach(updateSender)
  }
  
  private def updateSender(sender:Sender) {   
    sender.update(answers)
  }
  
  def addSender(sender:Sender) {
    senders += sender
    if (answers != null) {
      //updating the new sender
      updateSender(sender)
    }
  }
  
  def getEnv = env
  
  def getAnswers = answers 
  
  def getSenders = senders.toList

  /*
  override def hashCode() = env.hashCode ^ 2324334
  
  override def equals(that:Any) = {
    if (that == null || !that.isInstanceOf[Request]) false
    else {
      val request = that.asInstanceOf[Request]
      this.env.equals(request.getEnv)
    }
  }
  */
}

class Sender(rc:RequestContainer, paramType:Type) {
  def update(answers:ContainerNode){
    rc.addAnswer(paramType, answers)
  }
}

class InitialSender extends Sender(null, null) {
  private var answers:ContainerNode = null
  
  override def update(answers:ContainerNode) { 
    this.answers = answers
  }
  
  def getAnswers = answers
}

class RequestContainer(request:Request, decls:List[Declaration], paramTypes:List[Type]) {
  
  //TODO: Delete. We probably don't need this flag any more.
  //The reason is that Container is notified at most n times, where n is paramTypes.lenght
  private var answerPropagated:Boolean = false
  
  private var answers = scala.collection.mutable.Map.empty[Type, ContainerNode]
  
  tryToPropagateAnswer()
  
  def addAnswer(paramType:Type, answer:ContainerNode) {
    if (answers.contains(paramType))
      throw new Exception("Answers already contian an answer at: "+paramType+" in "+this.getClass.getName)
    else answers += (paramType -> answer)
    
    tryToPropagateAnswer()
  }
  
  private def tryToPropagateAnswer() {
    if(!answerPropagated && fullyAnswerd){
      //Debug("In RequestContainer.tryToPropagateAnswer: ", decls(0))
      
      request.addAnswer(new SimpleNode(decls, answers))
      answerPropagated = true
    }
  }
  
  def sendRequests(requests:Requests) {
    val oldEnv = request.getEnv
    
    //Debug("In RequestContainer.sendRequests: ", decls(0))
    
    paramTypes.foreach {
      paramType =>
        val builder = new DeltaEnvironmentBuilder(oldEnv)
        builder.create(Type.paramSetType(paramType))
        val newEnv = builder.produceEnvirionment
        
        //Debug("SentType: ", Type.returnType(paramType))
        //Debug("SentEnv: "+newEnv)
        requests.addSender(Type.returnType(paramType), newEnv, new Sender(this, paramType))
    }
  }
  
  def isAnswerPropagated = answerPropagated
  
  def fullyAnswerd = answers.size == paramTypes.length

}