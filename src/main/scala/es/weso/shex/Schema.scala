package es.weso.shex

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.rdf._
import es.weso.shex.ShapeSyntax._
import es.weso.shex.ShapeParser._
import es.weso.shex.PREFIXES._
import scala.util.parsing.input.Positional
import scala.util.{ Try, Success, Failure }
import es.weso.monads.Result
import org.slf4j._
import es.weso.utils.IO._

/**
 * The following definitions follow: http://www.w3.org/2013/ShEx/Definition
 *
 */

case class Schema(
    pm: PrefixMap, shEx: ShEx) extends Positional {

  override def toString(): String = {
    ShapeDoc.schema2String(this)(pm)
  }

  def getLabels(): List[Label] = {
    if (shEx.start == None) shEx.rules.map(_.label).toList
    else List(shEx.start.get)
  }

  def addAny: Schema =
    this.copy(pm = pm.addPrefix("shex", sh_IRI),
      shEx = shEx.copy(rules = shEx.rules :+ ShapeSyntax.anyShape)
    )

}

object Schema {

  def fromString(cs: CharSequence): Try[(Schema, PrefixMap)] = {
    ShapeParser.parse(cs) match {
      case s @ Success(_) => s
      case Failure(t) => Failure(throw new Exception("Parsing schema: " + t.getMessage))
    }
  }

  def fromFile(fileName: String): Try[(Schema, PrefixMap)] = {
    for (
      cs <- getContents(fileName); (schema, prefixMap) <- Schema.fromString(cs)
    ) yield (schema, prefixMap)
  }

}
