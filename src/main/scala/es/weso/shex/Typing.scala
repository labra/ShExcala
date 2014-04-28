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
 
}

object Typing {

  def emptyTyping : Typing = Typing(Map[IRI,Set[IRI]]())

}