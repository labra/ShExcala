package es.weso.shex

import scala.util._

case class SchemaVocabulary(name: String) {
 override def toString = s"$name"
}

object SchemaVocabulary {

object SHEX extends SchemaVocabulary("SHEX")
object SHACL extends SchemaVocabulary("SHACL")
  
val availableVocabularies : List[SchemaVocabulary] = 
    List(SHACL, SHEX)
    
val vocabularyNames : List[String] = 
   availableVocabularies.map(_.name)

lazy val default : SchemaVocabulary = SHEX
  
lazy val defaultVocabularyName = default.name
  
def lookup(key:String): Try[SchemaVocabulary] = {
  val keyUpper = key.toUpperCase
  availableVocabularies.find(_.name == keyUpper) match {
      case None => Failure(new Exception (s"lookup vocabulary: $keyUpper not found in: $vocabularyNames"))
      case Some(x) => Success(x)
    }
  }
  
}