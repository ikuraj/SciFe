package scife.enumeration.benchmarks

import org.scalameter._

import reporting._
import execution._
import Key._

class KoratParser(benchName: String) {

  def file = "./tmp/korat/" + benchName
  
  def parse = {
    val source = scala.io.Source.fromFile(file)

    try {
      val lines = source.getLines
      
      val data =
        for (line <- lines) yield {
          val parts = line.split("\t")
          val size = parts(0).toInt
          val runs = parts.toSeq.tail.map( _.toDouble * 1000 )
          
          val complete =
            if (runs.size > 0 && runs.size < 3)
              runs ++ Seq.fill(3 - runs.size)(runs.last)
            else runs
          
          (size -> complete)
        }

      data.toMap
    } finally {   
      source.close()
    }
    
  }
  
}
