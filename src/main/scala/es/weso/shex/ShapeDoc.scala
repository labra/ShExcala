package es.weso.shex

import scala.collection.immutable.StringOps
import es.weso.rdfNode.IRI
import es.weso.shex.ShapeSyntax._
import scala.text._
import Document._
import es.weso.parser.PrefixMap

case class ShapeDoc(pm: PrefixMap) {
  
  def schemaDoc(s: Schema) : Document = {
    pmDoc(s.pm) :/:
    rulesDoc(s.rules)
  }

  def pmDoc(pm: PrefixMap) : Document = {
    pm.map.foldLeft(empty: Document)(
        (d, x) => d :/: text("prefix ") :: x._1 :: space :: 
        		  text("<") :: text(x._2.str) :: text(">") 
    )  
  }
  
  def rulesDoc(rules: Seq[Shape]) : Document = {
    seqDocWithSep(rules, "\n", shapeDoc)
  }

  def shapeDoc(shape: Shape): Document = {
    labelDoc(shape.label) :: 
    space :: "{" :: space ::
    nest(3,group( ruleDoc(shape.rule))) :: space :: 
    text("}")  
  }

  def labelDoc(label: Label): Document = {
    label match {
      case IRILabel(iri) => iriDoc(iri)
      case BNodeLabel(id) => text(id.toString)
    }
  }
  
  def ruleDoc(rule: Rule) : Document = {
    rule match {
      case r : ArcRule => arcRuleDoc(r)
      case AndRule(conjoints) => seqDocWithSep(conjoints,",",ruleDoc)
      case OrRule(disjoints) => seqDocWithSep(disjoints,"|",ruleDoc)
      case GroupRule(rule,opt,a) => 
          text("(") :: space :: 
    	  nest(3,ruleDoc(rule)) :: space :: 
    	  text(")") :: 
    	  (if(opt) text("?") else empty) :: space :: 
    	  actionDoc(a)
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
      case ValueSet(s) => "(" :/: nest(3,seqDocWithSep(s," ",iriDoc)) :/: text(")")
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
    text(iri2String(i))  
  }
  
  def actionDoc(a : Seq[Action]) : Document = empty
  
  def pairDoc(d1: Document, d2: Document) : Document = 
    "(" :: d1 :: "," :: d2 :: ")" :: empty

  def space : Document = text(" ")
  
  def seqDocWithSep[A](s : Seq[A], 
      sep: String,
      toDoc : A => Document) : Document = {
    if (s.isEmpty) empty
    else
      s.tail.foldLeft(toDoc(s.head))(
          (d:Document, x:A) => d :: sep :/: toDoc(x) 
      )
  }


  def shape2String(shape: Shape) : String = 
    prettyPrint(shapeDoc(shape))

  def rules2String(rs: Seq[Shape]): String = {
    prettyPrint(rulesDoc(rs))
  }
  
  def schema2String(s: Schema): String = {
    prettyPrint(schemaDoc(s))
  }

  def iri2String(iri: IRI): String = {
 
    def startsWithPredicate(p:(String, IRI)): Boolean = {
      iri.str.startsWith(p._2.str)
    }
    
    pm.map.find(startsWithPredicate) match {
      case None => "<" ++ iri.str ++ ">"
      case Some(p) => p._1 ++ ":" ++ iri.str.stripPrefix(p._2.str) 
    }
  }

  /**
   * Generic function for pretty printing 
   */
  def prettyPrint(d: Document) : String = {
	  val writer = new java.io.StringWriter
	  d.format(1, writer)
	  writer.toString
  }

  

}

