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
import es.weso.utils.Logging
import es.weso.monads.Passed
import es.weso.monads.Failure

/**
 * Shape validator using Backtracking that tracks remaining triples
 * It reflects the axiomatic semantics
 */
trait ShapeValidatorRemaining extends ShapeValidator with Logging {

  override def id = "Validator Remaining"

  override def matchRule(ctx: Context,
    g: Set[RDFTriple],
    rule: Rule): Result[Typing] = ???

  case class State(
    typing: Typing,
    checked: Set[RDFTriple],
    remaining: Set[RDFTriple])

  def matchR(ctx: Context, g: Set[RDFTriple], rule: Rule): Result[State] =

    rule match {
      case AndRule(r1, r2) => for (
        (g1, g2) <- parts(g); State(t1, cs1, rs1) <- matchR(ctx, g1, r1); State(t2, cs2, rs2) <- matchR(ctx, g2, r2)
      ) yield State(t1 combine t2, cs1, rs1) // TODO...check that defn... 
    }

  /* Converts ranges to Ands/Opts
  * Example: range(2,4,x) = x, x, x?, x?
  * This function is not used 
  */
  def range(m: Int, n: Int)(r: Rule): Rule = {
    require(m >= 0, "range: m must not be negative")
    require(n >= m, "range: n (" + n + ") must be bigger than m (" + m + ")")
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

object ShapeValidatorRemaining extends ShapeValidatorRemaining {

  override def id = "Validator by Backtracking taking into account remaining triples 1.0"

  override def matchRule(
    ctx: Context,
    g: Set[RDFTriple],
    rule: Rule): Result[Typing] = {
    ???
  }
}