package insynth.util.format

import insynth.structures.{ ContainerNode, SimpleNode }
import insynth.load.Declaration
import java.io.PrintWriter
import java.io.FileWriter
import java.io.ByteArrayOutputStream

object TreePrinter {
 
  def apply(fileName:String, msg:String){
    val out = new PrintWriter(new FileWriter(fileName))
    out.println
    out.println(msg)
    out.flush
    out.close
  }
  
  def apply(fileName:String, msg:String, decls:List[Declaration]){
    val out = new PrintWriter(new FileWriter(fileName))
    
    out.println("Initial decls: ")
    decls.foreach{
      decl => out.println(decl)
    }
    
    out.println
    out.println(msg)
    out.flush
    out.close
  }
  
  def apply(fileName:String, answer:ContainerNode, decls:List[Declaration]){
    val out = new PrintWriter(new FileWriter(fileName))
    
    out.println("Initial decls: ")
    decls.foreach{
      decl => out.println(decl)
    }
    
    out.println    
    printAnswer(out, answer)
    out.flush
    out.close
  }
  
  def printAnswerAsXML(out: PrintWriter, answer: ContainerNode, depth: Int) {
    printAnswerAsXML(out, answer, "query", Set.empty, depth)
  }  
  
  private def printAnswerAsXML(out: PrintWriter, answer: ContainerNode, tpe: String, set: Set[SimpleNode], depth:Int) {
    println("printAnswerAsXML called")
    if(depth > 0) {
    	out.println("<container size = '" + answer.getNodes.size + "' type='" + tpe + "' >")
      answer.getNodes.foreach{
        simpleNode =>
          if (!set.contains(simpleNode)) {          
            out.println("""<simple visited="no">""")
            out.println(simpleNode.getDecls.map("<decl name='" + _ + "'/>").mkString("\n","\n\t,","\n"))
            for ((tpe, container) <- simpleNode.getParams) {
              printAnswerAsXML(out, container, tpe.toString, set + simpleNode, depth-1)
            }
            out.println("</simple>")
          } else {
            out.println("""<simple visited="yes">""")
            simpleNode.getDecls.map("<decl name='" + _ + "'/>").mkString("\n","\n\t,","\n")
            out.println("</simple>")
          }
      }
    	out.println("</container>")	
    }
    out.flush
    
  }
  
  def apply(fileName:String, answer:ContainerNode, decls:List[Declaration], depth:Int){
    val out = new PrintWriter(new FileWriter(fileName))
    
    out.println("Initial decls: ")
    decls.foreach{
      decl => out.println(decl)
    }
    
    out.println    
    printAnswerWithDepth(out, answer, depth)
    out.flush
    out.close
  }  
    
  def apply(answer:ContainerNode, depth:Int): String = {
    val byteArrayOutputStream = new ByteArrayOutputStream()
    val out = new PrintWriter(byteArrayOutputStream)
        
    out.println    
    printAnswerWithDepth(out, answer, depth)
    out.flush
    out.close
    
    byteArrayOutputStream.toString
  }
  
  def apply(fileName:String, msg:String, answer:ContainerNode, decls:List[Declaration]){
    val out = new PrintWriter(new FileWriter(fileName))
    
    out.println("Initial decls: ")
    decls.foreach{
      decl => out.println(decl)
    }
    
    out.println
    out.println(msg)
    out.println    
    printAnswer(out, answer)
    out.flush
    out.close
  }  
  
  def apply(fileName:String, msg:String, answer:ContainerNode, decls:List[Declaration], depth:Int){
    val out = new PrintWriter(new FileWriter(fileName))
    
    out.println("Initial decls: ")
    decls.foreach{
      decl => out.println(decl)
    }
    
    out.println
    out.println(msg)
    out.println    
    printAnswerWithDepth(out, answer, depth)
    out.flush
    out.close
  }
  
  def apply(answer:ContainerNode, decls:List[Declaration]){
    val out = new PrintWriter(System.out)
    
    out.println("Initial decls: ")
    decls.foreach{
      decl => out.println(decl)
    }
    
    out.println
    
    printAnswer(out, answer)
    out.flush
  }
 
  private def printAnswer(out:PrintWriter, answer:ContainerNode) {
    out.println
    out.println("Solution:")
    
    printAnswer(out, answer, Set.empty[SimpleNode], 0)
  }
  
  private def printAnswerWithDepth(out:PrintWriter, answer:ContainerNode, depth:Int) {
    out.println
    out.println("Solution:")
    
    printAnswerWithDebth(out, answer, Set.empty[SimpleNode], 0, depth)
  }  
  
  private def printAnswer(out:PrintWriter, answer:ContainerNode, set:Set[SimpleNode], length:Int) {
      answer.getNodes.foreach{
        simpleNode =>
          if (!set.contains(simpleNode)){          
            printlnDeclsWithIndention(out, simpleNode.getDecls, length)
            for ((tpe, container) <- simpleNode.getParams) {
              val tpeName = "["+tpe.toString+ "]"
              printlnWithIndention(out, tpeName, length)
              printlnWithIndention(out, "|", length + 4)
              printlnWithIndention(out, "V", length + 4)
              printAnswer(out, container, set + simpleNode, length + 4)
            }
          } else {
            printWithIndention(out, "Visited ", length)
            printlnDeclNamesWithIndention(out, simpleNode.getDecls, 0)
          }
      }
  }
  
  private def printAnswerWithDebth(out:PrintWriter, answer:ContainerNode, set:Set[SimpleNode], length:Int, depth:Int) {
    if(depth > 0){
      answer.getNodes.foreach{
        simpleNode =>
          if (!set.contains(simpleNode)){          
            printlnDeclNamesWithIndention(out, simpleNode.getDecls, length)
            for ((tpe, container) <- simpleNode.getParams) {
              val tpeName = "["+tpe.toString+ "]"
              printlnWithIndention(out, tpeName, length)
              printlnWithIndention(out, "|", length + 4)
              printlnWithIndention(out, "V", length + 4)
              printAnswerWithDebth(out, container, set + simpleNode, length + 4, depth-1)
            }
          } else {
            printWithIndention(out, "Visited ", length)
            printlnDeclNamesWithIndention(out, simpleNode.getDecls, 0)
          }
      }
    }
  }
  
  private def printlnWithIndention(out:PrintWriter, text:String, length:Int){
    for(i <- 0 until length) out.print(" ")
    out.println(text)
  }
  
  private def printWithIndention(out:PrintWriter, text:String, length:Int){
    for(i <- 0 until length) out.print(" ")
    out.print(text)
  }
  
  private def printlnDeclsWithIndention(out:PrintWriter, decls:List[Declaration], length:Int){
    printlnWithIndention(out, decls.mkString("Decls[",",","]"), length)
  }
  
  private def printlnDeclNamesWithIndention(out:PrintWriter, decls:List[Declaration], length:Int){
    printlnWithIndention(out, decls.map(_.toString).mkString("Decls[",",","]"), length)
  }
}