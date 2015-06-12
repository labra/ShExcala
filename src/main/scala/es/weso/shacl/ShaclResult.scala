package es.weso.shacl

import scalaz._
import Scalaz._

trait ShaclResult {
  
  type Result[A] = ValidationNel[String,Stream[A]]
  
}