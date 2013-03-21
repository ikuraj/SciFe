package insynth
package testdomain

import insynth.query._
import insynth.engine.InitialSender
import insynth.structures.SuccinctType

case class TestQuery(override val inSynthRetType: SuccinctType, decl: TestDeclaration, sender: InitialSender)
extends Query(inSynthRetType) {
  
  def getSolution = sender.getAnswers
  
  def getDeclaration = decl
  
  def getSender = sender
  
}