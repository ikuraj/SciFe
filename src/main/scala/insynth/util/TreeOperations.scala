package insynth.util

import java.io._

object ProofTreeOperations {
	import insynth.structures._

	def size(n: Node): Int = size(n, Set())
	
	private def size(n: Node, visited: Set[Node]): Int = {
	  if (visited contains n) 
	  	0
  	else n match {
	    case sn: SimpleNode =>
	      sn.getDecls.size + (0 /: sn.getParams) {
	      	(res, pair) => res + size(pair._2, visited + n)
      	}
	    case cn: ContainerNode =>
	      (0 /: cn.getNodes) {
	        (res, node) => res + size(node, visited + n)
        }
	  }
	}
	
	case class StringNode(name: String, nodes: Set[StringNode] = Set())
	
	def checkInhabitants(cn: ContainerNode, names: StringNode): Boolean = {	  
  	(for (outerInnerNode <- cn.getNodes; innerContainer <- outerInnerNode.getParams.values;
			innerNodes = innerContainer.getNodes;	innerNode <- innerNodes)
  	  yield
      	checkInhabitants(innerNode, names)
  	).reduce(_ || _)
	}
	
	def checkInhabitants(sn: SimpleNode, names: StringNode): Boolean = {
	  val visibleNames = 
	    for (decl <- sn.getDecls)
	      yield decl.getSimpleName
	  
	  names match {
	    case StringNode(currentName, set) if !set.isEmpty =>
	      if (visibleNames contains currentName)
	        (for (innerStringNode <- set)
	          yield 
	          	(for (innerContainer <- sn.getParams.values; innerNodes = innerContainer.getNodes;
	          		innerNode <- innerNodes)
	          	  yield
		            	checkInhabitants(innerNode, innerStringNode)
	          	).reduce(_ || _)
        	).reduce(_ && _)
	      else
	        false
	    case StringNode(currentName, _) =>
	      visibleNames contains currentName
	  }
	}
	
	def getSubtrees(sn: SimpleNode, names: StringNode): Set[SimpleNode] = {
		  val visibleNames = 
		    for (decl <- sn.getDecls)
		      yield decl.getSimpleName
		  
		  if (visibleNames contains names.name) {
			  if (!names.nodes.isEmpty) 
	        (for (innerStringNode <- names.nodes)
	          yield 
	          	(for (innerContainer <- sn.getParams.values; innerNodes = innerContainer.getNodes;
	          		innerNode <- innerNodes)
	          	  yield
		            	getSubtrees(innerNode, innerStringNode)
	          	).reduce(_ | _)
        	).reduce(_ | _)
		    else
		      Set(sn)
		  }
		  else Set.empty
	}
	
	def breadthFirstSearchPrint(sn: SimpleNode): String = {
	  val stringWriter = new StringWriter
	  breadthFirstSearchPrint(sn, new PrintWriter(stringWriter, true))
	  stringWriter.getBuffer.toString
	}
	
	def breadthFirstSearchPrint(sn: SimpleNode, out: PrintWriter) = {
	  var visited: Set[SimpleNode] = Set.empty
	  var currentList: List[SimpleNode] = List.empty
	  var nextLevelList: List[SimpleNode] = List(sn)
	  
	  var level = -1
	  
	  while (!nextLevelList.isEmpty) {
	    level += 1
	    currentList = nextLevelList
	    nextLevelList = List.empty
	    
	    for (node <- currentList; decl <- node.getDecls) {
	      visited += node
	      
	      for (_ <- 0 to level) out.print(' ')
	      	out.println(decl.toString)	      
	    }
	    
	    for (node <- currentList; (tpe, cn) <- node.getParams;
	    	innerNode <- cn.getNodes; if ! (visited contains innerNode)) {
	      nextLevelList +:= innerNode
	    }
	  }
	}
}