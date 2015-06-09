package scife
package enumeration
package util

import scife.{ enumeration => e }
import scife.util._
import scala.language.postfixOps
import logging._
import org.scalameter.api._

trait Benchmarker extends PerformanceTest {

  override def reporter: Reporter =
    Reporter.Composite(
      new RegressionReporter(
        RegressionReporter.Tester.Accepter(),
        RegressionReporter.Historian.Complete()),
      // do not embed data into js
      HtmlReporter(true))

  override def executor: Executor = SeparateJvmsExecutor(
    Executor.Warmer.Default(),
    Aggregator.average,
    new Executor.Measurer.Default)
    
  override def persistor = new SerializationPersistor

}
