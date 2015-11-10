package es.weso.utils

import org.slf4j._
import org.apache.log4j.LogManager
import org.apache.log4j.Level

trait Verbosity extends Logging {

  var isVerbose = true
  
  def verbose(msg: String): Unit = {
    if (isVerbose) {
      log.info(msg)
    }
  }
  
  def setVerbosity(verbosity: Boolean): Unit = {
    isVerbose = verbosity
    if (isVerbose) {
      setInfo()
    } else {
      setError()
    }
  }
  
}