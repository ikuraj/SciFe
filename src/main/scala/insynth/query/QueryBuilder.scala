package insynth
package query

import insynth.structures._

abstract class QueryBuilder(protected val tpe: SuccinctType) {
  require(tpe != null)
    
  val succinctReturnType = BottomType
  val succinctType = Arrow(TSet(tpe), succinctReturnType)
  
  def getQuery: Query
  
}