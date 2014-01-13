package es.weso.shex

import es.weso.rdfNode.IRI
import es.weso.shex.AbstractSyntax._
import scala.text._
import Document._

object PrettyPrint {
  
  def schemaDoc(schema: Schema) : Document = ???
  def shapeDoc(shape: Shape): Document = {
    labelDoc(shape.label) :: 
    space :: "{" :: space ::
    nest(3,group( ruleDoc(shape.rule))) :: space :: 
    text("}")  
  }

  def labelDoc(label: Label): Document = {
    label match {
      case IRILabel(iri) => text(iri.toString)
      case BNodeLabel(id) => text(id.toString)
    }
  }
  
  def ruleDoc(rule: Rule) : Document = {
    rule match {
      case r : ArcRule => arcRuleDoc(r)
      case AndRule(conjoints) => seqDocWithSep(conjoints,",")
      case OrRule(disjoints) => seqDocWithSep(disjoints,"|")
      case GroupRule(rule,opt,a) => ???
    }
  }

  def arcRuleDoc(arc: ArcRule) : Document = {
    nameClassDoc(arc.n) :: space ::
    valueClassDoc(arc.v) :: space ::
    cardinalityDoc(arc.c) :: space ::
    actionDoc(arc.a)
  }

  def nameClassDoc(n : NameClass) : Document = {
    n match {
      case NameTerm(t) => iriDoc(t)
      case NameAny(excl) => ???
      case NameStem(s) => ???
    }
  }

  def valueClassDoc(v: ValueClass) : Document = {
    v match {
      case ValueType(vtype) => iriDoc(vtype)
      case ValueSet(s) => "(" :/: nest(3,seqDocWithSep(s," ")) :/: text(")")
      case ValueAny(stem) => ???
      case ValueStem(stem) => ???
      case ValueReference(l) => "@" :: labelDoc(l)
    }    
  }
  
  def cardinalityDoc(c : Cardinality): Document = {
    c match {
      case Star => text("*")
      case Plus => text("+")
      case Opt  => text("?")
      case Default => empty
      case _ => pairDoc(text(c.min.toString), maxDoc(c.max)) 
    }
  }
  
  def maxDoc(m : Either[Integer,Unbound]): Document = {
    m match {
      case Left(n) => text(n.toString)
      case Right(_) => text("") // Todo: check specification how to express ranges of type (m,unbound)
    }
  }

  def iriDoc(i : IRI): Document = {
    text("<") :: text(i.str) :: text(">")  
  }
  
  def actionDoc(a : Seq[Action]) : Document = empty
  
  def pairDoc(d1: Document, d2: Document) : Document = 
    "(" :: d1 :: "," :: d2 :: ")" :: empty

  def space : Document = text(" ")
  
  def seqDocWithSep[A](s : Seq[A], sep: String) : Document = {
    if (s.isEmpty) empty
    else {
      ??? // s.fold
    }
  }

  def prettyPrint(d: Document) : String = {
	  val writer = new java.io.StringWriter
	  d.format(1, writer)
	  writer.toString
  }

  
  def prettyPrint(shape: Shape) : String = 
    prettyPrint(shapeDoc(shape))
    
}
