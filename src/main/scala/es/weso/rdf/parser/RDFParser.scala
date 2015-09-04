package es.weso.rdf.parser

import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import scala.util._
import es.weso.rdf._
import es.weso.shacl.Schema
import es.weso.shacl.PREFIXES._
import es.weso.rdfgraph.statements.RDFTriple
import es.weso.rdf.PrefixMap
import es.weso.utils.{Success => TrySuccess}

case class RDFParserException(msg: String)
  extends Exception(s"RDFParserException: " + msg)

trait RDFParser {

  type RDFParser[a] = (RDFNode, RDFReader) => Try[a]


  def hasPredicateWithSubject(n: RDFNode, p: IRI, rdf: RDFReader): Boolean = {
    rdf.triplesWithSubjectPredicate(n, p).size > 0
  }

  def iriFromPredicate(p: IRI): RDFParser[IRI] = { (n, rdf) =>
    for {
      node <- objectFromPredicate(p)(n, rdf)
    } yield node match {
      case i: IRI => i
      case _ => throw RDFParserException("Value of predicate " + p + " must be a IRI but it is: " + node)
    }
  }

  def stringFromPredicate(p:IRI): RDFParser[String] = { (n,rdf) =>
    for {
      obj <- objectFromPredicate(p)(n,rdf)
    } yield {
      obj match {
        case StringLiteral(str) => str
        case _ => throw RDFParserException("Value of predicate " + p + " must be a string literal but it is: " + obj)
      }
    }
  }
  
  def stringFromPredicateOptional(p: IRI): RDFParser[Option[String]] = 
    optional(stringFromPredicate(p))

  def objectFromPredicateOptional(p: IRI): RDFParser[Option[RDFNode]] = 
    optional(objectFromPredicate(p))
  
  
  def rdfType: RDFParser[RDFNode] = { (n,rdf) =>
    for {
      ts <- objectsFromPredicate(rdf_type)(n,rdf)
    } yield 
    if (ts.size == 1) 
      ts.head
    else 
      throw RDFParserException("Type for node " + n + " is not single. Found types = " + ts)
  } 
    
    
  def rdfTypes: RDFParser[Set[RDFNode]] =  
    objectsFromPredicate(rdf_type)

  def objectFromPredicate(p: IRI): RDFParser[RDFNode] = { (n, rdf) =>
    val ts = rdf.triplesWithSubjectPredicate(n, p)
    ts.size match {
      case 0 => fail("objectFromPredicate: Not found triples with subject " + n + " and predicate " + p)
      case 1 => Success(ts.head.obj)
      case _ => fail("objectFromPredicate: More than one value from predicate " + p + " on node " + n)
    }
  }

  def objectsFromPredicate(p: IRI): RDFParser[Set[RDFNode]] = { (n, rdf) =>
    val triples = rdf.triplesWithSubjectPredicate(n, p)
    // println("ObjectsFromPredicate " + p + ". node = " + n + " triples =" + triples)
    Success(objectsFromTriples(triples))
  }

  def rdfList: RDFParser[List[RDFNode]] = { (n, rdf) =>
    n match {
      case `rdf_nil` => Success(List())
      case x => {
        println("rdfList...n = " + n)
        for {
          elem <- objectFromPredicate(rdf_first)(n, rdf)
          next <- objectFromPredicate(rdf_rest)(n, rdf)
          ls <- rdfList(next, rdf)
        } yield (elem :: ls)
      }
    }
  }

  def rdfListForPredicate(p: IRI): RDFParser[List[RDFNode]] = { (n, rdf) =>
    println("rdfList...predicate..." + p)
    for {
      value <- {
        val v = objectFromPredicate(p)(n, rdf)
        println("value..." + v)
        v
      }
      ls <- {
        println("value found..." + value)
        rdfList(value, rdf)
      }
    } yield ls
  }

  def integerLiteralForPredicate(p: IRI): RDFParser[Integer] = { (n, rdf) =>
    val ts = rdf.triplesWithSubjectPredicate(n, p)
    ts.size match {
      case 0 => fail("integerLiteralFromPredicate: Not found triples with subject " + n + " and predicate " + p)
      case 1 => getIntegerLiteral(ts.head)
      case _ => fail("integerLiteralFromPredicate: More than one value from predicate " + p + " on node " + n)
    }
  }

  def getIntegerLiteral(t: RDFTriple): Try[Integer] = {
    t.obj match {
      case l: IntegerLiteral => Success(l.int)
      // TODO: case l: DatatypeLiteral(lexicalForm,datatype) => ...
      case _                 => fail("getIntegerLiteral: Object " + t.obj + " must be a literal")
    }
  }

  def hasNoRDFType(t: IRI): RDFParser[Boolean] = { (n, rdf) =>
    for {
      declaredTypes <- objectsFromPredicate(rdf_type)(n, rdf)
    } yield !declaredTypes.contains(t)
  }

  def hasRDFType(t: IRI): RDFParser[Boolean] = { (n, rdf) =>
    for {
      declaredTypes <- objectsFromPredicate(rdf_type)(n, rdf)
    } yield declaredTypes.contains(t)
  }

  def hasSomeRDFType(ts: Set[IRI]): RDFParser[Boolean] = { (n, rdf) =>
    for {
      declaredTypes <- objectsFromPredicate(rdf_type)(n, rdf)
    } yield (declaredTypes.map(_.toIRI).diff(ts)).size > 0
  }

  def optional[A](parser:RDFParser[A]): RDFParser[Option[A]] = { (n,rdf) =>
    parser(n,rdf) match {
      case Success(v) => Success(Some(v))
      case Failure(e) => Success(None)
    }
  }
  
  // TODO: Move the following code to a generic RDFParser 
  // This code could be more generic
  def someOf[A](ps: Seq[RDFParser[A]]): RDFParser[A] = { (n, rdf) =>
    {
      ps.foldLeft(fail("someOf: none of the RDFParsers passed")) {
        case ((s: Try[A], parser)) =>
          s match {
            case Success(_) => s
            case Failure(_) => parser(n, rdf)
          }
      }
    }
  }

/*    def mapParser[A](
    ls: List[RDFNode],
    parser: RDFParser[A]): RDFParser[List[A]] = { (n, rdf) =>
    ls match {
      case Nil => Success(Nil)
      case x :: xs => for {
        y <- parser(x, rdf)
        ys <- mapParser(xs, parser)(n, rdf)
      } yield y :: ys
    }
  } */

  def group[A](
      parser: RDFParser[A], 
      nodes: Seq[RDFNode]
   ): RDFParser[Seq[A]] = { (n, rdf) =>
    {
      val empty: Seq[A] = List()
      nodes.foldLeft(Success(empty)) {
        case (s, node) => {
          s match {
            case Success(rs) => {
              parser(node, rdf) match {
                case Success(a) => Success(a +: rs)
                case Failure(f) => throw new Exception("group: Unimplemented parsing node " + node + " Exception: " + f.getMessage) // TODO
              }
            }
            case _ => s
          }
        }
      }
    }
  }

   def oneOf[A](parsers: Seq[RDFParser[A]]): RDFParser[A] = { (n, rdf) =>
    {
      val r = parsers.foldLeft(fail("oneOf: none of the RDFParsers passed")) {
        case ((current: Try[A], parser)) =>
          current match {
            case Success(_) => {
              parser(n, rdf) match {
                case Success(_) => fail("oneOf: More than one parser passes")
                case Failure(_) => current
              }
            }
            case Failure(_) => parser(n, rdf)
          }
      }
      r
    }
  }

 
 // TODO: Move this general purpose method elsewhere
  def fail[A](str: String): Try[A] = {
    Failure(RDFParserException(str))
  }

  def subjectsWithType(t: RDFNode, rdf: RDFReader): Set[RDFNode] = {
    subjectsFromTriples(rdf.triplesWithPredicateObject(rdf_type, t))
  }

  def subjectsWithProperty(pred: IRI, rdf: RDFReader): Set[RDFNode] = {
    subjectsFromTriples(rdf.triplesWithPredicate(pred))
  }

  def subjectsFromTriples(triples: Set[RDFTriple]): Set[RDFNode] = {
    triples.map { case RDFTriple(s, _, _) => s }
  }

  def objectsFromTriples(triples: Set[RDFTriple]): Set[RDFNode] = {
    triples.map { case RDFTriple(_, _, o) => o }
  }

}