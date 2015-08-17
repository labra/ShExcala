package es.weso.shacl

import es.weso.rdfgraph.statements.RDFTriple
import es.weso.rdfgraph.nodes._
import es.weso.shacl.Shacl._
import es.weso.rdf.PrefixMap
import es.weso.utils.PrefixMapUtils._
import es.weso.shacl.ShaclDoc._

sealed abstract class ValidationError {
  def show(implicit pm:PrefixMap): String 
}

final case class NoMatchValueSet(n: RDFNode, vs: ValueSet) extends ValidationError {
  override def show(implicit pm: PrefixMap): String = 
    "No match valueSet " + showRDFNode(n)(pm) + " ~ values: " + valueSet2String(vs)(pm)
}

final case class NoNodeKind(n: RDFNode, nk: NodeKind) extends ValidationError {
  override def show(implicit pm: PrefixMap): String = 
    "No match nodeKind " + showRDFNode(n)(pm) + " ~ nodeKind: " + nodeKind2String(nk)
}

final case class NoMatchLiteralDatatype(l: Literal, ld: Datatype) extends ValidationError {
  override def show(implicit pm: PrefixMap): String = 
    "no match Literal datatype. Literal: " + l + " ~ Datatype: " + datatype2String(ld)(pm)
}

final case class NoLiteral_MatchLiteralDatatype(obj: RDFNode,ld: Datatype) extends ValidationError {
  override def show(implicit pm: PrefixMap): String = 
    "no Literal to match literalDatatype. Object: " + showRDFNode(obj)(pm) + " ~ Datatype: " + datatype2String(ld)(pm)
}

final case class NoTypingFound(obj: RDFNode,label: Label) extends ValidationError {
  override def show(implicit pm: PrefixMap): String = 
    "Node " + showRDFNode(obj)(pm) + " has negative shape: " + label2String(label)(pm)
}