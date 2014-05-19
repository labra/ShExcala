package es.weso.shex

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._

import es.weso.parser.PrefixMap
import scala.util.parsing.input.Positional


case class Typing(map:Map[IRI,Set[IRI]]) {

 def addType(key:IRI,value:IRI): Option[Typing] = {
   if (map contains key) {
     Some(Typing(map.updated(key,map(key) + value)))  
   } else {
     Some(Typing(map + ((key,Set(value)))))
   }
 }

 def rmType(key:IRI,value:IRI): Option[Typing] = {
   if ((map contains key) && (map(key) contains value)) {
     val newSet = map(key) - value
     if (newSet.isEmpty) {
       Some(Typing(map - key))
     } else {
       Some(Typing(map.updated(key,newSet)))
     }
   } else None
 }
 
 def hasType(iri: IRI): Set[IRI] = {
   if (map contains iri) map(iri)
   else Set()
 }
 
 def combine(other: Typing): Typing = {
   Typing(map ++ other.map)
 }

 def hasTypes(iri:IRI, iris:Set[IRI]): Boolean = {
   hasType(iri) == iris 
 }

   def showTyping(implicit pm: PrefixMap): String = {
    val sb = new StringBuilder
    for (is <- map) {
      sb ++= (showIRI(is._1) + " -> " + showIRIs(is._2) + "\n")
    }
    sb.toString
  }

  // TODO: Refactor to put these 2 definitions in RDF
  private def showIRI(iri: IRI)(implicit pm: PrefixMap): String = {
    "<" + iri.str + ">"
  }
  
  private def showIRIs(iris: Set[IRI])(implicit pm:PrefixMap): String = {
    val sb = new StringBuilder
    sb ++= "("
    for (iri <- iris) {
      sb ++= (showIRI(iri) + " ")
    }
    sb ++= ")"
    sb.toString
  }

}

object Typing {

  def emptyTyping : Typing = Typing(Map[IRI,Set[IRI]]())

}