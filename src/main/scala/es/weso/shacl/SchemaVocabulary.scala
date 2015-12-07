package es.weso.shacl

import scala.util._

case class SchemaVocabulary(name: String) {
 override def toString = s"$name"
}

object SchemaVocabulary {

object SHACL extends SchemaVocabulary("SHACL")
object SHEX extends SchemaVocabulary("SHEX")
  
val availableVocabularies : List[SchemaVocabulary] = 
    List(SHACL, SHEX)
    
val availableNames : List[String] = 
   availableVocabularies.map(_.name)

lazy val default : SchemaVocabulary = SHEX
  
lazy val defaultSchemaVocabulary = default.name
  
def lookup(key:String): Try[SchemaVocabulary] = {
  val keyUpper = key.toUpperCase
  availableVocabularies.find(_.name == keyUpper) match {
      case None => Failure(new Exception (s"lookup vocabulary: $keyUpper not found in: $availableNames"))
      case Some(x) => Success(x)
    }
  }
  
/*  def get(key:String): SchemaVocabulary = {
    lookup(key).getOrElse(default)
  } */
}