package es.weso.shex

import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.{ Literal, RDFNode }
import es.weso.shex.ShExDoc.{ datatype2String, label2String, nodeKind2String, valueSet2String }
import es.weso.utils.PrefixMapUtils.showRDFNode
import es.weso.validating.ConstraintError

abstract class ValidationError[A](msg:String) extends ConstraintError[A] {
  def show(implicit pm:PrefixMap): String 
}

/*final case class MsgError(msg: String) extends ValidationError(msg) {
  override def show(implicit pm:PrefixMap): String = {
    s"Message: $msg"
  }
}*/

final case class ExcError(e: Throwable) extends ValidationError(e.getMessage) {
  override def show(implicit pm:PrefixMap): String = {
    s"Exception: $e"
  }

  def msg = e.getMessage
}

final case class NoMatchValueSet(n: RDFNode, vs: ValueSet) extends ValidationError("NoMatchValueSet") {
  override def show(implicit pm: PrefixMap): String = 
    "No match valueSet " + showRDFNode(n)(pm) + " ~ values: " + valueSet2String(vs)(pm)

  override def msg = s"No match Value set $n, value set: $vs"
}

final case class NoNodeKind(n: RDFNode, nk: NodeKind) extends ValidationError("NoNodeKind") {
  override def show(implicit pm: PrefixMap): String = 
    "No match nodeKind " + showRDFNode(n)(pm) + " ~ nodeKind: " + nodeKind2String(nk)

  override def msg = s"No match nodeKind $nk with node $n"
}

final case class NoMatchLiteralDatatype(l: Literal, ld: Datatype) extends ValidationError("NoMatchLiteralDatatype") {
  override def show(implicit pm: PrefixMap): String = 
    "no match Literal datatype. Literal: " + l + " ~ Datatype: " + datatype2String(ld)(pm)

  override def msg = s"no match Literal datatype. Literal: $l ~ Datatype: $ld"
}

final case class NoLiteral_MatchLiteralDatatype(obj: RDFNode,ld: Datatype) extends ValidationError("NoLiteral_MatchLiteralDatatype") {
  override def show(implicit pm: PrefixMap): String = 
    "no Literal to match literalDatatype. Object: " + showRDFNode(obj)(pm) + " ~ Datatype: " + datatype2String(ld)(pm)

  override def msg = s"no Literal to match datatype. Node: $obj ~ Datatype: $ld"
}

final case class NoTypingFound(obj: RDFNode,label: Label) extends ValidationError("NoTypingFound") {
  override def show(implicit pm: PrefixMap): String = 
    "Node " + showRDFNode(obj)(pm) + " has negative shape: " + label2String(label)(pm)

  override def msg = s"No typing found for node $obj with label $label"
}