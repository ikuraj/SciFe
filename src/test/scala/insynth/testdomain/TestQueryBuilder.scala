package insynth
package testdomain

import insynth.query._
import insynth.structures._
import insynth.engine._

case class TestQueryBuilder(goalType: SuccinctType) extends QueryBuilder(goalType) {
  
  val inSynthRetType = BottomType
  val inSynthType = Arrow(TSet(tpe), inSynthRetType)
  
  val domainRetType = Atom(BottomType)
  val domainType = Function(List(Atom(goalType)), domainRetType)
  
  def getQuery = TestQuery(inSynthRetType, TestDeclaration(domainType), new InitialSender())
  
}