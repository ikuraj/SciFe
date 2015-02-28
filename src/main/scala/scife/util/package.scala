package scife

package object util {
  
  // classes that log inside the package can have the log switched off at compile time
  type HasLogger =
//    logging.DummyLogger
    logging.HasLogger
  
  type CountedLogger = logging.CountedLogger
  
  type ProfileLogger = logging.ProfileLogger

}
