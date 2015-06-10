package es.weso.shex

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.rdfgraph.statements._
import es.weso.shex.ShapeSyntax._
import es.weso.shex.Typing._
import es.weso.shacl.PREFIXES._
import es.weso.monads.Result._
import es.weso.monads.Result
import es.weso.rdf.PrefixMap
import scala.util.parsing.input.Positional
import es.weso.rdf._
import es.weso.shex.Context._
import org.slf4j._
import scala.util.matching.Regex
import es.weso.utils.Logging

trait ShapeValidator extends Logging {

  def id: String // Abstract

  def matchAll(ctx: Context): Result[Typing] = {

    def matchWithTyping(iri: IRI, typing: Typing)(shape: Shape): Result[Typing] = {
      log.debug("matchSome. iri: " + iri.toString +
        "\n--typing: " + typing.toString +
        "\n--shape: " + shape.toString)
      for (t <- matchShape(ctx, iri, shape))
        yield typing.combine(t)
    }

    def matchSomeWithTyping(iri: IRI, typing: Typing): Result[Typing] = {
      log.debug("matchSome. iri: " + iri.toString + ". typing: " + typing.toString)
      Result.passSome(ctx.getShapes, matchWithTyping(iri, typing))
    }

    log.debug("ctx: " + ctx.toString)
    log.debug("iris: " + ctx.getIRIs)
    Result.passAll(ctx.getIRIs, emptyTyping, matchSomeWithTyping)

  }

  def matchShape(ctx: Context, node: RDFNode, shape: Shape): Result[Typing] = {
    log.debug("matchShape, node: " + node + " with shape " + shape.label)
    val triples = ctx.triplesAround(node)
    log.debug("matchShape, triplesAround(" + node + ") = " + triples)
    for {
      ctx1 <- ctx.addTyping(node, shape.label.getNode) 
      t <- matchRule(ctx1, triples, shape.rule)
    } yield t
  }

  def showTriples(g: Set[RDFTriple]): Unit = {
    for (t <- g) {
      log.debug("   " + t)
    }
  }

  def showRule(r: Rule): String = {
    ShapeDoc.rule2String(r)(PrefixMap.empty)
  }

  def matchName(ctx: Context, pred: IRI, n: NameClass): Result[Boolean] = {
    log.debug("Matchname: " + pred + " ~ " + n)

    n match {

      case NameTerm(t) => {
        if (pred == t) unit(true)
        else {
          val msg = "matchName: iri=" + pred + " does not match name=" + t
          log.debug(msg)
          failure(msg)
        }
      }

      case NameAny(excl) => {
        if (matchStems(excl, pred)) {
          val msg = "matchName: iri= " + pred + " appears in excl= " + excl
          log.debug(msg)
          failure(msg)
        } else unit(true)
      }

      case NameStem(s) => {
        if (s.matchStem(pred)) unit(true)
        else {
          val msg = "matchName: iri= " + pred + " does not match stem= " + s
          log.debug(msg)
          failure(msg)
        }
      }
    }
  }

  /**
   * abstract method that can be overriden with different algorithms: derivatives, backtracking, etc.
   */
  def matchRule(ctx: Context,
    g: Set[RDFTriple],
    rule: Rule): Result[Typing]

  def matchValue(ctx: Context, obj: RDFNode, v: ValueClass): Result[Typing] = {

    log.debug("MatchValue: " + obj + " ~ " + v)

    v match {

      case ValueType(v) => {
        for (
          b <- matchType(obj, v); if (b)
        ) yield ctx.typing
      }

      case ValueSet(s) => {
        val evalSet = Result.passSome(s.toList, matchValueObject(obj))
        if (evalSet.run.get.filter(x => x == true).size > 0) {
          unit(ctx.typing)
        } else {
          val msg = "matchValue: obj" + obj + " is not in set " + s
          log.debug(msg)
          failure(msg)
        }
      }

      case ValueAny(excl) => {
        if (matchStems(excl, obj)) {
          val msg = "matchValue, value any: iri= " + obj + " appears in excl= " + excl
          log.debug(msg)
          failure(msg)
        } else {
          unit(ctx.typing)
        }
      }

      case ValueStem(s) => {
        if (s.matchStem(obj)) unit(ctx.typing)
        else {
          val msg = "matchValue, value stem: iri = " + obj + " does not have stem = " + s
          log.debug(msg)
          failure(msg)
        }
      }

      case ValueReference(l) =>
        if (ctx.containsType(obj, l.getNode)) unit(ctx.typing)
        else
          for (
            shape <- ctx.getShape(l); newT <- matchShape(ctx, obj, shape)
          ) yield newT
    }
  }

  def matchType(obj: RDFNode, vtype: RDFNode): Result[Boolean] = {
    log.debug("MatchType: " + obj + " ~ " + vtype)
    obj match {
      case lit: Literal => {
        if (vtype == sh_Literal ||
          vtype == sh_NonIRI ||
          vtype == sh_NonBNode ||
          lit.dataType == vtype) {
          unit(true)
        } else unit(false)
      }
      case iri: IRI => {
        if (vtype == sh_IRI ||
          vtype == sh_NonLiteral ||
          vtype == sh_NonBNode) unit(true)
        else unit(false)
      }
      case bnode: BNodeId =>
        if (vtype == sh_BNode ||
          vtype == sh_NonIRI ||
          vtype == sh_NonLiteral) unit(true)
        else unit(false)
    }

  }

  def matchValueObject(node: RDFNode)(vo: ValueObject): Result[Boolean] = {
    log.debug("MatchValueObject: " + node + " ~ " + vo)
    vo match {
      case RDFNodeObject(n) => unit(n == node)
      case LangObject(lang) => node match {
        case l: LangLiteral => matchLang(lang, l)
        case _ => failure("matchVallueObject: value object: " + vo + " does not match " + node)
      }
      case RegexObject(r, None) =>
        node match {
          case l: StringLiteral => matchRegex(r, l)
          case _ => failure("matchValueObject: Regex " + r + " does not match node " + node)
        }
      case RegexObject(r, Some(lang)) =>
        node match {
          case l: LangLiteral =>
            for (
              _ <- matchRegex(r, l); b <- matchLang(lang, l)
            ) yield b
          case _ => failure("matchValueObject: Regex " + r + " with lang " + lang + " does not match node " + node)
        }
      case NoObject(obj) => {
        for (
          b <- matchValueObject(node)(obj)
        ) yield !b
      }
      case OrObject(o1, o2) => {
        for (
          b1 <- matchValueObject(node)(o1); b2 <- matchValueObject(node)(o2)
        ) yield b1 || b2
      }
    }

  }

  def matchRegex(r: Regex, lit: Literal): Result[Boolean] = {
    r.findFirstIn(lit.lexicalForm) match {
      case None => failure("matchValue: regex " + r.toString + " does not match literal " + lit.lexicalForm)
      case Some(_) => unit(true)
    }
  }

  def matchLang(lang: Lang, lit: LangLiteral): Result[Boolean] = {
    // TODO: Improve language matching
    if (lang == lit.lang)
      unit(true)
    else failure("Lang " + lang + " does not match lang of literal " + lit)
  }

}