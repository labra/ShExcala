package es.weso.rbe

case class RbeException(msg: String) extends Exception(s"RbeException: " + msg)
