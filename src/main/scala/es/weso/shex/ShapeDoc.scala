package es.weso.shex

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import scala.collection.immutable.StringOps
import es.weso.shex.ShapeSyntax._
import scala.text._
import Document._
import es.weso.parser.PrefixMap
import arq.iri
import org.slf4j.LoggerFactory

case class ShapeDoc(pm: PrefixMap) {

  def schemaDoc(s: Schema) : Document = {
    pmDoc(s.pm) :/:
    shExDoc(s.shEx) 
  }

  def shExDoc(sh: ShEx) : Document = {
    rulesDoc(sh.rules) // TODO: start
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
      case r : RevArcRule => "^" :/: revArcRuleDoc(r)
      case AndRule(e1,e2) => "(" :/: ruleDoc(e1) :/: text(",") :/: ruleDoc(e2) :/: text(")")
      case OrRule(e1,e2) => "(" :/: ruleDoc(e1) :/: text("|") :/: ruleDoc(e2) :/: text(")")
      case OneOrMore(r) => "(" :/: ruleDoc(r) :/: text(")+")
      case NotRule(r) => "!" :/: ruleDoc(r) 
      case ActionRule(r,a) => "(" :/: ruleDoc(r) :/: text(") %") :/: actionDoc(a)
      case NoRule => text(" ")
    }
  }

  def arcRuleDoc(arc: ArcRule) : Document = {
    nameClassDoc(arc.n) :: space ::
    valueClassDoc(arc.v) 
  }
  
  def revArcRuleDoc(arc: RevArcRule) : Document = {
    nameClassDoc(arc.n) :: space ::
    valueClassDoc(arc.v) 
  }

  def nameClassDoc(n : NameClass) : Document = {
    n match {
      case NameTerm(t) => iriDoc(t)
      case NameAny(excl) => {
        if (excl.isEmpty) text("NameAny.")
        else 
          text("-") :/: nest(3,setDocWithSep(excl," ",iriStemDoc))
      }
      case NameStem(s) => iriStemDoc(s)
    }
  }

  def valueClassDoc(v: ValueClass) : Document = {
    v match {
      case ValueType(v) => rdfNodeDoc(v)
      case ValueSet(s) => "(" :/: nest(3,seqDocWithSep(s," ",valueObjectDoc)) :/: text(")")
      case ValueAny(stem) => {
        if (stem.isEmpty) text(".")
        else 
          text("-") :/: nest(3,setDocWithSep(stem," ",iriStemDoc))
      }
      case ValueStem(stem) => iriStemDoc(stem)
      case ValueReference(l) => "@" :: labelDoc(l)
    }    
  }

  def valueObjectDoc(vo: ValueObject): Document = {
    vo match {
      case RDFNodeObject(n) => rdfNodeDoc(n)
      case LangObject(lang) => text("@") :/: text(lang.lang)
      case RegexObject(r,None) => "/" :/: text(r.toString) :/: text("/")
      case RegexObject(r,Some(lang)) => "/" :/: text(r.toString) :/: text("/") :/: "@" :/: text(lang.lang)
    }
  }

 def rdfNodeDoc(n : RDFNode): Document = {
    text(rdfNode2String(n))  
  }

  def iriDoc(i : IRI): Document = {
    text(iri2String(i))  
  }
  
  def iriStemDoc(i : IRIStem): Document = {
    text(iri2String(i.iri))  
  }

  def actionDoc(a : Seq[Action]) : Document = 
    ???
  
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

  def setDocWithSep[A](s : Set[A], 
      sep: String,
      toDoc : A => Document) : Document = {
    if (s.isEmpty) empty
    else
      s.tail.foldLeft(toDoc(s.head))(
          (d:Document, x:A) => d :: sep :/: toDoc(x) 
      )
  }

  def rules2String(rs: Seq[Shape]): String = {
    prettyPrint(rulesDoc(rs))
  }
  
  def rdfNode2String(n: RDFNode): String = {
    n match {
      case BNodeId(id) => "_:" + id
      case iri: IRI => iri2String(iri)
      case l:Literal => l.toString
    }
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

  def schema2String(s: Schema): String = {
    prettyPrint(schemaDoc(s))
  }

  def shape2String(shape: Shape) : String = 
    prettyPrint(shapeDoc(shape))

  def schema2String(shEx: ShEx) : String = 
    prettyPrint(shExDoc(shEx))
}

object ShapeDoc {
}


