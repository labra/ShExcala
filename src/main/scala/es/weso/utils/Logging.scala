package es.weso.utils

import org.slf4j._
import org.apache.log4j._
import scala.collection.JavaConversions._ 

trait Logging {

  lazy val log = LogManager.getRootLogger
  val appenders = log.getAllAppenders
  val appender : ConsoleAppender = log.getAppender("stdout").asInstanceOf[ConsoleAppender]

  def setDebug() {
    val debug = Level.DEBUG
    log.setLevel(debug)
    appender.setThreshold(debug)
  }
  
  def setInfo() {
    val info = Level.INFO
    log.setLevel(info)
    appender.setThreshold(info)
  }
  
  def setError() {
    val error = Level.ERROR
    log.setLevel(error)
    appender.setThreshold(error)
  }
  
  def info(msg: String) {
    log.info(msg)
  }
  
  def debug(msg: String) {
    log.debug(msg)
  }

}