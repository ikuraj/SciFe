package insynth.util.format

import scala.text.Document._
    
import insynth.structures._

// enable implicit conversions
import scala.language.implicitConversions

class FormatSuccinctType(tpe: SuccinctType) extends Formatable {
  import insynth.util.format.FormatHelpers._
  
  override def toDocument = toDocument(tpe)
  
  def toDocument(tpe: SuccinctType): scala.text.Document = {    
    tpe match {
      case Const(name) => name
      case Arrow(TSet(paramList), returnType) => 
        paren(seqToDoc(paramList, ",", toDocument(_: SuccinctType))) :: "→" :: toDocument(returnType)
      case BottomType => "⊥"
      case Instance(name, list) => name :: "[" :: seqToDoc(list, ",", toDocument(_: SuccinctType)) :: "]" 
      case _ => throw new UnsupportedOperationException
    }
  }
  
}

object FormatSuccinctType {
  def apply(tpe: SuccinctType) = new FormatSuccinctType(tpe)
  
  implicit def succinctTypeToDocument(tpe: SuccinctType) = FormatSuccinctType(tpe).toDocument
}