package es.weso.utils

import org.slf4j._
import org.apache.log4j.LogManager
import org.apache.log4j.Level


trait Logging {

  lazy val log = LogManager.getLogger("Logging")
  
  def setDebug() {
    log.setLevel(Level.DEBUG)
  }

}