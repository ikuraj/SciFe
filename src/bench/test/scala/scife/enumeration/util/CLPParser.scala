package scife.enumeration.benchmarks

import org.scalameter._

import reporting._
import execution._
import Key._

class CLPParser(benchName: String) {

  def file = "./tmp/clp/" + benchName

  val regex = """Size :([0-9]*) Time: ([0-9]*) ms""".r
  
  def parse = {
    val source = scala.io.Source.fromFile(file)

    try {
      val lines = source.getLines
      
      val data =
        for (line <- lines) yield {
          
          line match {
            case regex(size, run) =>
              val complete = Seq.fill(3)(run.toDouble)
          
              (size.toInt -> complete)
          }
        }

      data.toMap
    } finally {   
      source.close()
    }
    
  }
  
}