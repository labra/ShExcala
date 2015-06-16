package es.weso.shex

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import es.weso.rdfgraph.statements._
import es.weso.shex.ShapeSyntax._
import es.weso.shex.Typing._
import es.weso.monads.Result._
import es.weso.monads.Result
import es.weso.rdf.PrefixMap
import es.weso.rdf._
import es.weso.shex.Context._
import org.slf4j._
import scala.util.matching.Regex
import es.weso.utils._
import es.weso.monads._

/**
 * Shape validator using Backtracking
 * It mimics the inference rule semantics but is computationally very expensive
 */
trait ShapeValidatorBacktracking extends ShapeValidator with Logging {

  override def id = "Validator by Backtracking 1.0"

  override def matchRule(
    ctx: Context,
    g: Set[RDFTriple],
    rule: Rule): Result[Typing] = {

    log.debug("matchRule " + showRule(rule))

    rule match {

      case AndRule(r1, r2) => for {
        (g1, g2) <- parts(g)
        t1 <- matchRule(ctx, g1, r1)
        t2 <- matchRule(ctx, g2, r2)
      } yield t1 combine t2

      case OrRule(r1, r2) =>
        matchRule(ctx, g, r1) orelse matchRule(ctx, g, r2)

      case PlusRule(r) => {
        matchRule(ctx, g, r) orelse
          (for (
            (g1, g2) <- parts(g); t1 <- matchRule(ctx, g1, r); t2 <- matchRule(ctx, g2, rule)
          ) yield t1 combine t2
          )
      }

      case StarRule(AnyRule) => {
        log.debug("matching with any *")
        unit(ctx.typing)
      }

      case StarRule(r) => {
        matchRule(ctx, g, EmptyRule) orelse matchRule(ctx, g, PlusRule(r))
      }

      case OptRule(r) => {
        matchRule(ctx, g, r) orelse matchRule(ctx, g, EmptyRule)
      }

      case EmptyRule =>
        if (g.isEmpty) unit(ctx.typing)
        else {
          val msg = "EmptyRule: graph non empty"
          log.debug(msg)
          failure(msg)
        }

      case NotRule(r) => {
        if (matchRule(ctx, g, r).isFailure) unit(ctx.typing)
        else {
          val msg = "NotRule: matches"
          log.debug(msg)
          failure(msg)
        }
      }

      case AnyRule => {
        log.debug("matching with any")
        if (g.size == 1) unit(ctx.typing)
        else
          failure("Any arc expected but found a graph with " + g.size + " triples")
      }

      case ArcRule(id, name, value) =>
        if (g.size == 1) {
          val triple = g.head
          for (
            b <- matchName(ctx, triple.pred, name); typing <- matchValue(ctx, triple.obj, value)
          ) yield typing
        } else {
          val msg = "Arc expected but zero or more than one triple found in graph:\n" + g.toString
          log.debug("fail: " + msg)
          failure(msg)
        }

      case RevArcRule(id, name, value) =>
        if (g.size == 1) {
          val triple = g.head
          for (
            b <- matchName(ctx, triple.pred, name); typing <- matchValue(ctx, triple.subj, value)
          ) yield typing
        } else {
          val msg = "RevArc expected one but zero or more than one triple found in graph:\n" + g.toString
          log.debug("fail: " + msg)
          failure(msg)
        }

      case RelationRule(id, value1, value2) =>
        if (g.size == 1) {
          val triple = g.head
          for (
            t1 <- matchValue(ctx, triple.subj, value1); t2 <- matchValue(ctx, triple.obj, value2)
          ) yield t1 combine t2

        } else {
          val msg = "RelationRule expected one but zero or more than one triple found in graph:\n" + g.toString
          log.debug("fail: " + msg)
          failure(msg)
        }

      case ActionRule(r, a) => {
        log.debug("Executing... " + a)
        unit(ctx.typing)
      }

      case RangeMinRule(m, r) => {
        if (m < 0) failure("RangeMin with negative value " + m)
        else if (m == 0 && g.size == 0) unit(ctx.typing)
        else {
          matchRule(ctx, g, AndRule(r, RangeMinRule(m - 1, r)))
        }
      }

      case RangeRule(m, n, r) => {
        if (m > n) failure("Range with bad interval (" + m + "," + n + ")")
        else if (m == 0)
          if (n == 0)
            matchRule(ctx, g, EmptyRule)
          else
            matchRule(ctx, g, AndRule(OptRule(r), RangeRule(0, n - 1, r)))
        else
          matchRule(ctx, g, AndRule(r, RangeRule(m - 1, n - 1, r)))
      }

      case FailRule(msg) =>
        failure(msg)

      case OpenRule(r) => {
        for (
          (g1, remaining) <- parts(g); t <- matchRule(ctx, g1, r)
        ) yield {
          t
        }
      }
    }

  }

  /* Converts ranges to Ands/Opts
  * Example: range(2,4,x) = x, x, x?, x?
  * This function is not used 
  */
  def range(m: Int, n: Int)(r: Rule): Rule = {
    require(m >= 0, "range: m must not be negative")
    require(n >= m, "range: n(" + n + ") must be bigger than m (" + m + ")")
    if (m == 0) {
      if (n == 0) EmptyRule
      else AndRule(OptRule(r), range(0, n - 1)(r))
    } else {
      AndRule(r, range(m - 1, n - 1)(r))
    }
  }

  def showTriples(
    ts: Set[RDFTriple],
    ctx: Context): String = {
    RDFTriples(ts, ctx.pm).serialize()
  }

}

object ShapeValidatorBacktracking extends ShapeValidatorBacktracking {

}