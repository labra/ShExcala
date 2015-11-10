package es.weso.rdf.validator

case class MainException(msg: String) 
  extends Exception(s"Exception in main: $msg")
