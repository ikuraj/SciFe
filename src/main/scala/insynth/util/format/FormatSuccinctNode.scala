package insynth.util.format

import scala.text.Document._
    
import insynth.structures._
import insynth.load.Declaration

case class FormatSuccinctNode(node: Node, levels: Int = -1) extends Formatable {
  override def toDocument = toDocument(node, Set.empty, 0)
  
  def toDocument(node: Node, visited: Set[Node], level: Int): scala.text.Document = {
    import FormatHelpers._
    import FormatSuccinctType.succinctTypeToDocument
    
    val simple = levels == level

    if (!simple)
    node match {
      case sn:SimpleNode =>
        "SimpleNode" :: nestedBrackets(
            "decls: " :/: nestedBrackets(seqToDoc(sn.getDecls, ",", { d: Declaration => strToDoc(d.toString) }))
            :/:
            "params: " :: break :: seqToDoc(sn.getParams.toList, ",", 
              { 
            	p:(SuccinctType, ContainerNode) => p._1 :: "->" ::
            	nestedBrackets(toDocument(p._2, visited + node, level + 1))
              }
            )
        )
      case cn:ContainerNode =>
        nestedBrackets(seqToDoc(cn.getNodes.toList, ",", 
        { sn:SimpleNode => 
          	if (visited contains sn) 
      		  "already visited (" :: sn.getDecls.head.toString :: ")"
      		else
      		  toDocument(sn, visited, level + 1)
        }
        ))        
    }
    
    else 
    node match {
      case sn:SimpleNode =>
        "SimpleNode*" :: paren(sn.getDecls.head.toString)
      case cn:ContainerNode =>
        "Container*" :: paren(cn.getNodes.size)  
    }
  }
}