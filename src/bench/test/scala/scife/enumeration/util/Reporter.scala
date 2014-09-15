package scife.enumeration.benchmarks

import org.scalameter._

import reporting._
import execution._
import utils._
import Key._

class SciFeReporter(inner: Reporter) extends Reporter {

  def extractData(tool: String, bench: String) = tool match {
    case "Korat" =>
      val parser = new KoratParser(bench)
      parser.parse
    case "CLP" =>
      val parser = new CLPParser(bench)
      parser.parse
  }

  def report(result: CurveData, persistor: Persistor) = {
    inner.report(result, persistor)
  }

  def report(results: Tree[CurveData], persistor: Persistor) = {
    val newResults =
      results map { cd =>
        if (cd.context.curve == "SciFe") cd
        else {
          val newMeasurements =
            for (m <- cd.measurements) yield {
              assert(m.params.axisData.size == 1)
              val size = m.params.axisData("size").asInstanceOf[Int]

              val externalData = extractData(cd.context.curve, cd.context.scopeList(1).replaceAll(" ", ""))

              if (externalData.contains(size) && !externalData(size).isEmpty) {
                val value = externalData(size).sum / externalData(size).size
                val complete = externalData(size)

                Some( Measurement(value, m.params, new Measurement.Data(complete, true), m.units) )
              } else None
            }

          CurveData(newMeasurements.flatten, cd.info, cd.context)
        }
      }

    inner.report(newResults, persistor)
  }

}
