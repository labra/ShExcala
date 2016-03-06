package es.weso.rbe

/**
 * RBE exceptions
 */
case class RbeException(msg: String) extends Exception(s"RbeException: " + msg)
