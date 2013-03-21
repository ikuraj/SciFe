package insynth.util.logging

import scala.collection.mutable.{ Map => MutableMap }

import com.typesafe.scalalogging.log4j.Logger
import org.apache.logging.log4j.LogManager

/** 
 * Factory for producing loggers
 */
object LoggerFactory {
  
  val logDirectory = "log"
  
  val loggerMap: MutableMap[String, Logger] = MutableMap.empty

  /**
   * returns appropriate logger according to the given string
   * e.g. if (className contains "package.clazz")
   * 				(Filter.Info, new SimpleFormatter(className) with StringLogger)
   *     	else
   *      	(Filter.Finest, new SimpleFormatter(className) with ConsoleLogger)
   * @param className name of a class to return the logger for
   * @return logger for the class
   */
  final def newLogger(className: String) =
	Logger(LogManager.getLogger(className))
    //(new DummyLogger, null)

}