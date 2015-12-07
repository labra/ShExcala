package es.weso.shacl

import scala.util._
import SchemaFormat._

case class SchemaLanguage(
   format: SchemaFormat, 
   vocabulary: SchemaVocabulary) {
 override def toString = s"$format/$vocabulary"
}

object SchemaLanguage {

  object SHEXC extends SchemaLanguage(SchemaFormat.SHEXC, SchemaVocabulary.SHEX)
  object SHEXJSON extends SchemaLanguage(SchemaFormat.JSONAST, SchemaVocabulary.SHEX)
  
  lazy val availableLanguages : Seq[SchemaLanguage] = {
    val schemaVocabularies = SchemaVocabulary.availableVocabularies
    val rdfCombinations = rdfFormats.map(f => schemaVocabularies.map(v => SchemaLanguage(f,v))).flatten
    Seq(SHEXC,SHEXJSON) ++ rdfCombinations 
  }
    
  val availableFormats: Seq[String] = {
    availableLanguages.map(_.format.toString).distinct
  }
  
  def availableVocabularies(format: String): Seq[SchemaVocabulary] = {
    val formatUpper = format.toUpperCase
    availableLanguages.filter(_.format == formatUpper).map(_.vocabulary)
  }
    
  val availables : Seq[(String,String)]= 
    availableLanguages.map(l => (l.format.toString,l.vocabulary.toString))

  lazy val default : SchemaLanguage = SHEXC
  
  def lookup(format:String, vocab: String): Try[SchemaLanguage] = {
    val formatUpper = format.toUpperCase
    val vocabUpper = vocab.toUpperCase
    availableLanguages.find(l => l.format.name == formatUpper && l.vocabulary.name == vocabUpper) match {
      case None => Failure(new Exception (s"$format/$vocab not found in available schema languages: $availables"))
      case Some(x) => Success(x)
    }
  }
  
 def lookupOnlyFormat(format: String): Try[SchemaLanguage] = {
   val shexcStr = SchemaFormat.SHEXC.name
   val jsonStr = SchemaFormat.JSONAST.name
   val formatUpper = format.toUpperCase
   
   formatUpper match {
     case `shexcStr` => Success(SHEXC)
     case `jsonStr` => Success(SHEXJSON)
     case _ => lookupIn(formatUpper, rdfFormats) match {
       case Some(f) => Success(SchemaLanguage(f, SchemaVocabulary.default))
       case None => Failure(new Exception(s"Cannot find a schema language for format $format")) 
     }
   }
 }
}