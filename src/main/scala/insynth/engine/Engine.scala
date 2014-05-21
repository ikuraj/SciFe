package insynth.engine

import insynth.engine.scheduler.Scheduler
import insynth.structures.{ SuccinctType => Type }
import insynth.query.Query

import insynth.util.TimeOut
import insynth.util.logging.HasLogger

class Engine(builder:InitialEnvironmentBuilder, query: Query, scheduler:Scheduler, timeout:TimeOut) extends HasLogger {
  assert(builder != null && query != null && scheduler != null && timeout != null)
  
  def run() = {
    info("engine run on query: " + query)
    
    //Adding query decl to the env
    builder.addDeclaration(query.getDeclaration)
    
    //Create request pool
    val requests = new Requests()
    
    //Register Scheduler as a listener of the request pool
    requests.registerListener(scheduler)
    
    //Initiate first request, that will find query decl and put it into Scheduler
    requests.addSender(query.getReturnType, builder.produceEnvirionment, query.getSender)

    timeout.start()
    while(!timeout.hasExpired() && !scheduler.hasFinished()){
      var ta = scheduler.next()
      ta.processRequests(requests)
    }
    
    query.getSolution
  }
}

