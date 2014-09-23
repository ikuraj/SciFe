package scife.util.logging

import scala.collection.mutable.{ Map => MutableMap }

import com.typesafe.scalalogging._
//import org.apache.logging.log4j.LogManager
//import org.apache.logging.slf4j.Log4jLogger
import org.slf4j.{ LoggerFactory => SLF4JLoggerFactory }

/**
 * Factory for producing loggers
 */
object LoggerFactory {

  val logDirectory = "log"

  val loggerMap: MutableMap[String, Logger] = MutableMap.empty

  /**
   * returns appropriate logger according to the given string
   * e.g. if (className contains "package.clazz")
   *        (Filter.Info, new SimpleFormatter(className) with StringLogger)
   *      else
   *        (Filter.Finest, new SimpleFormatter(className) with ConsoleLogger)
   * @param className name of a class to return the logger for
   * @return logger for the class
   */
  final def newLogger(className: String) =
    Logger( SLF4JLoggerFactory.getLogger(className) )
  //(new DummyLogger, null)

}
