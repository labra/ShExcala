package es.weso.utils

import org.slf4j._
import org.apache.log4j._
import scala.collection.JavaConversions._ 

trait Logging {
  
  lazy val log = LogManager.getRootLogger
  
  val appenders = log.getAllAppenders.toList.filter(_.isInstanceOf[ConsoleAppender]) 
  val appender = 
    if (log.getAppender("stdout") == null) {
      if (appenders.isEmpty) {
        new ConsoleAppender
      } else {
        appenders.head.asInstanceOf[ConsoleAppender]
      }
    } else
       log.getAppender("stdout").asInstanceOf[ConsoleAppender]
  
  appender.setLayout(new PatternLayout("%m%n"))    

  def setDebug() {
    val debug = Level.DEBUG
    log.setLevel(debug)
    appender.setThreshold(debug)
  }
  
  def setInfo() {
    val info = Level.INFO
    log.setLevel(info)
    if (appender != null) appender.setThreshold(info)
  }
  
  def setError() {
    val error = Level.ERROR
    log.setLevel(error)
    if (appender != null) appender.setThreshold(error)
  }
  
  def info(msg: String) {
    log.info(msg)
  }
  
  def debug(msg: String) {
    log.debug(msg)
  }

}

object Logging {
  
  
  
}