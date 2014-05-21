package insynth.structures

// enable implicit conversions
import scala.language.implicitConversions
// enable postfix operators
import scala.language.postfixOps

/**
 * DomainType is here so that early reconstruction phases know
 * the structure of types without the need to know the actual
 * type 
 */
sealed abstract class DomainType {
  def toSuccinctType = DomainType.toSuccinctType(this)
}

case class Atom(st: SuccinctType) extends DomainType
case class Function(args: List[DomainType], retType: DomainType) extends DomainType

object DomainType {
    
  def toSuccinctType(domainType: DomainType): SuccinctType = {    		
		implicit def singletonList(x: SuccinctType) = List(x)

    domainType match {
      case Atom(st) => st
      case Function(froms, to) => transformFunction(to, froms)
    }
  }
  
  private def transformFunction(fun: DomainType, params: List[DomainType]): SuccinctType = fun match {
    case Function(froms, to) =>
      transformFunction(to, params ++ froms)
    case Atom(st) =>
      Arrow( TSet(params map this.toSuccinctType distinct), st )
  }
  
}