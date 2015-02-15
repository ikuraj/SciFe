package scife

package object enumeration {

  // classes that log inside the package can have the log switched off at compile time
  type HasLogger = //DummyLogger
    scife.util.logging.HasLogger

}
