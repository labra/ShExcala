package es.weso.shacl

import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.{ Literal, RDFNode }
import es.weso.shacl.ShaclDoc.{ datatype2String, label2String, nodeKind2String, valueSet2String }
import es.weso.utils.PrefixMapUtils.showRDFNode

sealed abstract class ValidationError {
  def show(implicit pm:PrefixMap): String 
}

final case class MsgError(msg: String) extends ValidationError {
  override def show(implicit pm:PrefixMap): String = {
    s"Message: $msg"
  }
}

final case class ExcError(e: Throwable) extends ValidationError {
  override def show(implicit pm:PrefixMap): String = {
    s"Exception: $e"
  }
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