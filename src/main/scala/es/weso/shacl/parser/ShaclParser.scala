package es.weso.shacl.parser

import es.weso.parser._
import es.weso.rdf._
import es.weso.shacl.PREFIXES._
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input._
import scala.util.parsing.input.Positional
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import scala.util.{ Try, Success => UtilSuccess, Failure => UtilFailure }
import org.slf4j._
import scala.util.{Failure => UtilFailure}
import scala.util.{Success => UtilSuccess}
import es.weso.shacl._
import es.weso.shacl.Cardinality._
import scala.annotation.tailrec
import scala.collection.immutable.LinearSeq



/**
 * Shacl parser. This parser follows
 * http://www.w3.org/2005/01/yacker/uploads/ShEx3/bnf
 */
trait ShaclParser
    extends Positional
    with RegexParsers
    with StateParser
    with W3cTokens 
    with TurtleParser {

  val log = LoggerFactory.getLogger("ShExParser")

  case class ShapeRule(label : Label,shape: Shape) 

  /**
   * Main entry point for parser
   */
  
 // [1]     shexDoc               ::= directive* startStatements? 
  def shexDoc: StateParser[ShapeParserState,Schema] = { s => 
    seqState(repS(directive), optState(startStatements))(s) ^^ {
      case (_ ~ None,s1) => 
        (Schema(s1.namespaces, SHACLSchema.empty),s1)
      case (_ ~ Some((as,rules)),s1) => {
        val shapes = rules2Map(rules) ++ s1.createdShapes
        val startLabel = if (s1.starts.isEmpty) None
                         else Some(s1.starts.last)
        val shaclSchema = 
            SHACLSchema.empty.copy(
                shapes = shapes,
                start = startLabel, 
                startActions = as,
                valueClasses = s1.createdValueClasses)   
        (Schema(s1.namespaces, shaclSchema), s)
      }
    }
  }
  
  // startStatements :: beginGroup statement* 
  def startStatements: StateParser[ShapeParserState,(Actions,List[Option[ShapeRule]])] = { s =>
    seqState(beginGroup,repS(statement))(s) ^^ {
      case ((startActions,statement) ~ statements, s1) => ((startActions,statement :: statements),s1)
    }
  }

  // TODO: Refactor the following code...type declarations look ugly
  // beginGroup :: (start | shape | startActions)
  def beginGroup: StateParser[ShapeParserState,(Actions,Option[ShapeRule])] = { s =>
   ( start(s) <~ opt(WS) ^^ { 
     case s1 => {
      val ret : (Actions,Option[ShapeRule]) = (Actions.empty,None)
      (ret, s1)
     }
   }
   | shapeRule(s) <~ opt(WS) ^^ { 
     case (shape, s1) => {
       val ret: (Actions,Option[ShapeRule]) = (Actions.empty,Some(shape))
       (ret, s1)
     } }
   | startActions(s) <~ opt(WS) ^^ { case (m,s1) => {
    val ret : (Actions,Option[ShapeRule]) = (m,None)
    (ret,s1) 
    } 
   }

   )
  } 
  
  def rules2Map(rules: List[Option[ShapeRule]]): Map[Label, Shape] = {
    rules.flatten.map(x => (x.label,x.shape)).toMap
  }


  // Rule [2]     statement             ::= directive | start | shape
  def statement: StateParser[ShapeParserState, Option[ShapeRule]] = { s => 
    (directive(s) <~ opt(WS) ^^ { case (_,s1) => (None, s1) }
//      | begin(s) ^^ { case (rule, s1) => (Some(rule), s1) }
    | start(s) <~ opt(WS) ^^ { case s1 => (None, s1) }
    | shapeRule(s) <~ opt(WS) ^^ { case (shape, s1) => (Some(shape), s1) }
    | valueClassDefinition(s) <~ opt(WS) ^^ { case ((label,vcd), s1) => (None, s1.addValueClassDefinition(label,vcd)) }
    )
  }
  
  def valueClassDefinition: StateParser[ShapeParserState,(Label,ValueClassDefinition)] = { s =>
    (valueClassLabel(s.namespaces) <~ symbol("=")) ~ (seqState(valueClass, semanticActions))(s) ^^ {
      case (lbl ~ ((vc ~ as,s1))) => {
        val vcd = ValueClassDefinition.fromValueClassActions(vc, as)
        ((lbl,vcd),s1.addValueClassDefinition(lbl,vcd))
      }
    }
  }

  // [3a]    valueClassLabel       ::= '$' iri
  def valueClassLabel: PrefixMap => Parser[Label] = { pm =>
    symbol("$") ~> iri(pm) ^^ { case iri => IRILabel(iri) }
  }

  def directive: StateParser[ShapeParserState,Unit] = { s => 
    for {
      s1 <- (prefixDirective(s) | baseDirective(s))
    } yield ((),s1) 
  }

/*  def begin: StateParser[ShapeParserState, ShapeRule] = { s =>
    token("begin") ~> opt(WS) ~> "=" ~> opt(WS) ~> {
      shapeExpr(s)
    } ^^ {
      case (shape, s1) => {
        (ShapeRule(IRILabel(IRI("begin")), shape), s1)
      }
    }
  } */

  // [4]     start                 ::= 'start' '=' ( shapeLabel | shapeDefinition semanticActions )
  def start(s: ShapeParserState): Parser[ShapeParserState] = {
    ignorecase("start") ~> opt(WS) ~> "=" ~> opt(WS) ~>
      ((label(s) ^^ {
        case (label, s1) => s1.addStart(label)
      }) 
      |(seqState(shapeDefinition, semanticActions)(s) ^^ {
        case (shapeDef ~ as, s1) => {
         val shape = mkShape(shapeDef).copy(actions = as)
         val (label,s2) = s1.newShape(shape)
         s2.addStart(label) 
        }
      })
      )
  }

  def baseDirective(s: ShapeParserState): Parser[ShapeParserState] = {
    (SPARQLBase | baseId) ^^ {
      case (iri) => s.newBase(s.baseIRI.resolve(iri))
    }
  }

  def prefixDirective(s: ShapeParserState): Parser[ShapeParserState] = {
    (SPARQLPrefix | prefixId) ^^ {
      case (prefix, iri) => s.addPrefix(prefix, iri)
    }
  }

  
  case class Inclusion(
      labels: Option[List[Label]],
      extras: Option[List[IRI]],
      closed: Boolean)
  object Inclusion {
    lazy val empty = Inclusion(
        labels = None, 
        extras = None, 
        closed = false)
  }
  
  lazy val closedInclusion = Inclusion.empty.copy(closed = true)
  
  def extrasInclusion(ps: List[IRI]) = 
    Inclusion.empty.copy(extras = Some(ps))
    
  def labelsInclusion(labels: List[Label]) = 
    Inclusion.empty.copy(labels = Some(labels))

  
  type ShapeDef = (List[Inclusion], ShapeExpr)

  def shapeRule: StateParser[ShapeParserState, ShapeRule] = { s => 
    opt(ignorecase("VIRTUAL")) ~ 
    seq3State(label, shapeDefinition,semanticActions)(s) ^^ { 
      case (virtual ~ (((label, shapeDef, as),s1))) => 
        (mkShapeRule(label,virtual.isDefined, shapeDef,as),s1) 
    }
  }
  
  
  def mkShape(shapeDef: ShapeDef): Shape = {
    val inherit = collectInherit(shapeDef)
    val extras = collectExtras(shapeDef)
    val closed = collectClosed(shapeDef)
    val shape = collectShape(shapeDef)
    Shape(shapeExpr = shape, 
            isClosed = closed, 
            isVirtual = false,
            inherit = inherit,
            extras = extras,
            actions = Actions.empty)
  }
  
  def mkShapeRule(
      label: Label, 
      isVirtual: Boolean,
      shapeDef: ShapeDef,
      as: Actions): ShapeRule = {
    val inherit = collectInherit(shapeDef)
    val extras = collectExtras(shapeDef)
    val closed = collectClosed(shapeDef)
    val shape = collectShape(shapeDef)
    ShapeRule(label, Shape(shapeExpr = shape, 
            isClosed = closed, 
            isVirtual = isVirtual,
            inherit = inherit,
            extras = extras,
            actions = as))
  }
  
  def collectInherit(sd: ShapeDef): List[Label] = {
    sd._1.filter(_.labels.isDefined).map(_.labels.get).flatten
  }
  
  def collectExtras(sd: ShapeDef): List[IRI] = {
    sd._1.filter(_.extras.isDefined).map(_.extras.get).flatten
  }
  
  def collectClosed(sd: ShapeDef): Boolean = {
    sd._1.filter(_.closed == true).length > 0
  }
  
  def collectShape(sd:ShapeDef): ShapeExpr = {
    sd._2
  }
  
  def shapeInclusions: StateParser[ShapeParserState,List[Inclusion]] = { 
    repS(shapeInclusion)
  }
  
  
  def shapeInclusion: StateParser[ShapeParserState,Inclusion] = { s =>
   ( ignorecase("CLOSED") ^^^ (closedInclusion,s)
   | ignorecase("EXTRA") ~> rep1State(pred)(s) ^^ {
     case (ps,s1) => (extrasInclusion(ps),s1)
   }
   | symbol("&") ~> rep1State(label)(s) ^^ {
     case (labels,s1) => (labelsInclusion(labels),s1)
   }
   )
  }
  
  def pred: StateParser[ShapeParserState,IRI] = { s =>
    opt(WS) ~> predicate(s) <~ opt(WS)^^ {
      case iri => (iri,s)
    }
  }
  
  def shapeDefinition: StateParser[ShapeParserState,ShapeDef] = { s => 
    seqState(shapeInclusions,shapeExpr)(s) ^^ {
      case (inclusions ~ shape,s1) => ((inclusions,shape),s1) 
    }  
  }
    
  def label: StateParser[ShapeParserState, Label] = { s =>
    opt(WS) ~>
      (iriBase(s.namespaces,s.baseIRI) ^^ {
        case iri => (IRILabel(iri), s)
      }
        | BlankNode(s.bNodeLabels) ^^ {
          case ((b, t1)) => (BNodeLabel(b), s.newTable(t1))
        })
  }

  def shapeExpr: StateParser[ShapeParserState, ShapeExpr] = { s =>
    opt(WS) ~> typeSpec(s) ^^ {
      case (shape, s1) => (shape, s1)
    }
  }

  // TODO: Rename to shapeDefinition
  def typeSpec: StateParser[ShapeParserState, ShapeExpr] = { s =>
    "{" ~> opt(WS) ~> opt(someOfExpr(s)) <~ opt(WS) <~ "}" ^^
      {
        case None            => (EmptyShape(), s)
        case Some((ors, s1)) => (ors, s1)
      }
  }
/*
  def oneOfExpr: StateParser[ShapeParserState, ShapeExpr] = { s =>
    seqState(someOfExpr,
      repS(arrowState(someOfExpr, symbol("|"))))(s) ^^
      {
        case (shape ~ List(), s1) => (shape, s1)
        case (shape ~ shapes, s1) => (OneOf(None, shape :: shapes), s1)
      }
  } */

  def someOfExpr: StateParser[ShapeParserState, ShapeExpr] = { s =>
    seqState(sequenceExpr,
      repS(arrowState(someOfExpr, symbol("|"))))(s) ^^
      {
        case (shape ~ List(), s1) => (shape, s1)
        case (shape ~ shapes, s1) => (SomeOf(None, shape :: shapes), s1)
      }

  }
  
  def sequenceExpr: StateParser[ShapeParserState, ShapeExpr] = { s => 
    seqState(idUnaryExpr,
      repS(arrowState(sequenceExpr, symbol(",")))
      )(s) <~ opt(",") ^^
      {
        case (shape ~ List(), s1) => (shape, s1)
        case (shape ~ shapes, s1) => (simplifyGroup(GroupShape(None, shape :: shapes)), s1)
      }
  }
  
  
  def simplifyGroup(s: ShapeExpr): ShapeExpr = {
    val xs = collectGroupShapes(s)
    if (xs.length > 1) {
      GroupShape(None,xs)
    } else s
  }
  
  def collectGroupShapes(s: ShapeExpr): List[ShapeExpr] = {
    s match {
      case GroupShape(None,List(x,rest)) => x :: collectGroupShapes(rest)
      case _ => List(s)
    }
  } 
  
  def idUnaryExpr: StateParser[ShapeParserState,ShapeExpr] = { s =>
   opt(id(s)) >> {
     case None => unaryExpr(s)
     case Some((idLabel,s1)) => unaryExpr(s1) ^^ {
       case (shape,s2) => (shape.addId(idLabel),s2)
     }
   } 
  }

  def unaryExpr: StateParser[ShapeParserState, ShapeExpr] = { s =>
    ( arc(s)
    | unaryGroup(s)
    | includeShape(s)
    )
  }
  
  def unaryGroup: StateParser[ShapeParserState,ShapeExpr] = { s =>
    for {
     (expr,s1) <- (symbol("(") ~> someOfExpr(s) <~ symbol(")"))
     card <- opt(cardinality)
     (annotations,s2) <- annotations(s1)
     (actions,s3) <- semanticActions(s2)
    } yield {
     if ((card == None || card.get == defaultCardinality) && annotations.isEmpty && actions.isEmpty)
       (expr,s3)
     else {
       // TODO: Simplify in case expr is a triple constraint with no cardinality...
       (RepetitionShape(None,expr,card.getOrElse(defaultCardinality),annotations,actions),s3)
     } 
       
    }
  }
  def id: StateParser[ShapeParserState,Label] = { s =>
    symbol("$") ~> label(s) ^^ {
      case (label,s1) => (label,s1)
    }
  }
  
  def includeShape: StateParser[ShapeParserState, ShapeExpr] = { s =>
    symbol("&") ~> label(s) ^^ {
      case (label,s1) => (IncludeShape(None,label),s1)
    }
  }
  
  def combine6[A,B,C,D,E,F,S](
      p1: StateParser[S,A],
      p2: StateParser[S,B],
      p3: StateParser[S,C],
      p4: StateParser[S,D],
      p5: StateParser[S,E],
      p6: StateParser[S,F]
      ):StateParser[S,(A,B,C,D,E,F)] = { s =>
    for {
      (a,s1) <- p1(s)
      (b,s2) <- p2(s1)
      (c,s3) <- p3(s2)
      (d,s4) <- p4(s3)
      (e,s5) <- p5(s4)
      (f,s6) <- p6(s5)
    } yield ((a,b,c,d,e,f),s6)
  }

  // [15]    tripleConstraint      ::= senseFlags? predicate valueClassOrRef cardinality? annotation* semanticActions
  def arc: StateParser[ShapeParserState, ShapeExpr] = { s =>
    val senseFlagsLifted: StateParser[ShapeParserState,Sense] = lift(senseFlags)
    val cardinalityLifted: StateParser[ShapeParserState,Cardinality] = lift(cardinality)
    combine6(
        senseFlagsLifted, 
        pred, 
        valueClassOrRef, 
        cardinalityLifted, 
        annotations, 
        semanticActions)(s) ^^ {
      case ((sense, p, v, c, annotations, actions),s1) => 
        ( TripleConstraint.empty.copy(
                id = None, 
                iri = p, 
                value = v, 
                card = c,
                inverse = sense.inverse,
                negated = sense.negated,
                annotations = annotations,
                actions = actions), s1)
    }
  }
  
  case class Sense(inverse: Boolean, negated: Boolean)
  
  def valueClassOrRef: StateParser[ShapeParserState,ValueClass] = { s =>
    ( valueClass(s) 
    | valueClassLabel(s.namespaces) ^^ { case lbl => (ValueClassRef(lbl),s)}
    )
  }

  def annotations: StateParser[ShapeParserState, List[Annotation]] = { s => 
    rep(annotation(s.namespaces,s.baseIRI)) ^^ {
      case as => (as,s)
    }
  }
  
  def annotation(pm: PrefixMap, base: IRI): Parser[Annotation] = {  
    symbol(";") ~> iriBase(pm,base) ~ iriOrLiteral(pm,base) ^^ {
      case iri ~ value => Annotation(iri,value)
    }
  }
  
  def iriOrLiteral(pm: PrefixMap, base: IRI): Parser[Either[IRI,Literal]] = { 
   opt(WS) ~> 
   ( iriBase(pm,base) ^^ { case i => Left(i) }
   | literal(pm) ^^ { case l => Right(l) }
   )
  }
  
  def senseFlags: Parser[Sense] = {
   opt ( symbol("^") ~ opt(symbol("!")) ^^ {
      case (_ ~ Some(_)) => Sense(inverse = true, negated = true)
      case (_ ~ None) => Sense(inverse = true, negated = false)
   } |
     symbol("!") ~ opt(symbol("^")) ^^ {
      case (_ ~ Some(_)) => Sense(inverse = true, negated = true)
      case (_ ~ None) => Sense(inverse = false, negated = true)
   } ) ^^ {
     case None => Sense(inverse = false, negated = false)
     case Some(sense) => sense
   }
  }

  def cardinality: Parser[Cardinality] =
    opt(repeatCount) ^^ {
      case Some(c) => c
      case None    => defaultCardinality
    }

  def valueClass: StateParser[ShapeParserState, ValueClass] = { s =>
    (ignorecase("IRI") ~> WS ~> 
        seqState(optState(groupShapeConstr), stringFacetsState)(s) ^^ {
      case (shapeConstr ~ facets,s1) => (IRIKind(shapeConstr, facets), s1)
    }
    | ignorecase("LITERAL") ~> WS ~> xsFacets(s) ^^ {
        case facets => (LiteralKind(facets), s)
    }
    | ignorecase("BNODE") ~> WS ~> 
        seqState(optState(groupShapeConstr), stringFacetsState)(s) ^^ {
        case (shapeConstr ~ facets,s1) => (BNodeKind(shapeConstr, facets), s1)
      }
      | ignorecase("NONLITERAL") ~> WS ~> 
              seqState(optState(groupShapeConstr), stringFacetsState)(s) ^^ {
        case (shapeConstr ~ facets,s1) => (NonLiteralKind(shapeConstr, facets), s1)        
      }
//      | ignorecase("ANY") ^^^ (AnyKind, s)
      | dot ^^^ (ValueClass.any, s)
      | iriFacetsChecked(s)
      | valueSet(s)
      | groupShapeConstr(s)
      | shapeDefinition(s) ^^ {
        case (shapeDef,s1) => {
         val shape = mkShape(shapeDef)
         val (label,s2) = s1.newShape(shape)
         (SingleShape(label),s2)
        }
      }
      )
  }
  
  def iriFacetsChecked:StateParser[ShapeParserState,Datatype] = { s =>
    parseCond(iriFacets, okFacets, "Datatype must pass facets")(s) ^^ {
      case (((iri,facets),s)) => (Datatype(iri,facets),s)
    }    
  }
  
  def iriFacets: StateParser[ShapeParserState,(IRI,List[XSFacet])] = { s =>
      opt(WS) ~> iriBase(s.namespaces,s.baseIRI) ~ xsFacets(s) <~ opt(WS) ^^ {
        case iri ~ facets => {
         ((iri, facets), s)
        }
      }
    }
  
  def okFacets(pair: (IRI, List[XSFacet])): Boolean = {
   XSFacet.ok_facets(pair._1,pair._2) 
  }
  
  def parseCond[A,S](
      p:StateParser[S,A], 
      cond: A => Boolean, 
      msg: String): StateParser[S,A] = { s =>
      p(s) >> {
        case ((v,s)) => if (cond(v)) {
          success((v,s))
        } else {
          failure(s"Failed parsing condition with value $v: Condition: $msg")
        }
      }
/*      for {
        (v,s) <- p(s)
        if (cond(v))
      } yield (v,s) */
    }

  def groupShapeConstr: StateParser[ShapeParserState,ShapeConstr] = { s =>
   rep1sepState(singleShapeConstr,symbol("AND"))(s) ^^ {
     case (shapes,s1) => 
       if (shapes.length == 1) (shapes.head,s1)
       else {
         val conj = mkConjShape(shapes)
         (conj,s1)
       }
   } 
  } 
  
  def mkConjShape(shapes: Seq[ShapeConstr]): ShapeConstr = {
    def getLabel(s: ShapeConstr): Label = {
      s match {
        case SingleShape(l) => l
        case _ => throw new Exception(s"Complex expression in ShapeConstr not handled yet: shapeConstr: $s")
      }
    }
    ConjShapeConstr(shapes.map(getLabel(_)))
  }
  
  def singleShapeConstr: StateParser[ShapeParserState,ShapeConstr] = { s =>
  ( token("@") ~> label(s) ^^ { case (label,s1) => (SingleShape(label), s1) }
  | token("!") ~> label(s) ^^ { case (label,s1) => (NotShape(label), s1) }
  )
  }
  
  def valueSet: StateParser[ShapeParserState, ValueSet] = { s => 
    (openSquareBracket ~>
      rep1sepState(s, valueObject, WS)
      <~ closeSquareBracket) ^^ {
        case (ls, s) => (ValueSet(ls), s)
      }
  }

  def openParen: Parser[String] = symbol("(")
  def closeParen: Parser[String] = symbol(")")
  def openSquareBracket: Parser[String] = symbol("[")
  def closeSquareBracket: Parser[String] = symbol("]")

  def valueObject(s: ShapeParserState): Parser[(ValueObject, ShapeParserState)] = {
    ( // symbol("-") ~> basicValueObject(s) ^^ { case ((vo, s)) => (NoObject(vo), s) } | 
      basicValueObject(s))
  }

  def basicValueObject: StateParser[ShapeParserState, ValueObject] = { s => 
    opt(WS) ~> (
        lift(iriRange(s.namespaces,s.baseIRI))(s) 
      | literal(s.namespaces) ^^ { case l => {
       (ValueLiteral(l), s) 
      } } 
      ) <~ opt(WS)
  }
    
  def valueAny(pm: PrefixMap, base: IRI): Parser[ValueObject] = {  
      dot ~> rep1(exclusion(pm,base)) ^^ {
        case exclusions => ValueAny(exclusions)
      }
  }
  
  def exclusion(pm: PrefixMap, base: IRI): Parser[Exclusion] = { 
    symbol("-") ~> iriBase(pm,base) ~ opt(symbol("~")) ^^ {
      case iri ~ None => Exclusion(iri,false)
      case iri ~ Some(_) => Exclusion(iri,true)
    } 
  }
  
  def iriRange(pm: PrefixMap, base: IRI): Parser[ValueObject] = {  
      iriOrStem(pm,base) | valueAny(pm,base) 
    } 
  
  def iriOrStem(pm: PrefixMap, base: IRI): Parser[ValueObject] = { 
      iriBase(pm,base) ~ (opt(symbol("~") ~> rep(exclusion(pm,base)))) ^^ {
        case iri ~ None => ValueIRI(iri)
        case iri ~ Some(exclusions) => StemRange(iri,exclusions)
      }
  }
    

  def xsFacetsState: StateParser[ShapeParserState, List[XSFacet]] = { s => 
    xsFacets(s) ^^ { case fs => (fs,s) }
  } 


  def xsFacets(s: ShapeParserState): Parser[List[XSFacet]] = {
    repsep(xsFacet(s), WS)
  }

  def stringFacetsState: StateParser[ShapeParserState, List[StringFacet]] = { s => 
    stringFacets(s) ^^ { case fs => (fs,s) }
  } 
    
    
  def stringFacets(s: ShapeParserState): Parser[List[StringFacet]] = {
    repsep(stringFacet(s), WS)
  }

  // TODO: Check differences between Shacl literals and Turtle literals
  def stringFacet(s: ShapeParserState): Parser[StringFacet] = {
    ((ignorecase("PATTERN") ~> opt(WS) ~> string) ^^ {
      case str => {
       Pattern(str) 
      }
    }
      | (symbol("~") ~> opt(WS) ~> string) ^^ Pattern
      | ignorecase("LENGTH") ~> integer ^^ { case n => Length(n) }
      | ignorecase("MINLENGTH") ~> integer ^^ { case n => MinLength(n) }
      | ignorecase("MAXLENGTH") ~> integer ^^ { case n => MaxLength(n) })
  }

  def numericFacet(s: ShapeParserState): Parser[NumericFacet] = {
    (numericRange(s) | numericLength(s))
  }

  def numericRange(s: ShapeParserState): Parser[NumericFacet] = {
    (ignorecase("MININCLUSIVE") ~> integer ^^ { case n => MinInclusive(n) }
      | ignorecase("MINEXCLUSIVE") ~> integer ^^ { case n => MinExclusive(n) }
      | ignorecase("MAXINCLUSIVE") ~> integer ^^ { case n => MaxInclusive(n) }
      | ignorecase("MAXEXCLUSIVE") ~> integer ^^ { case n => MaxExclusive(n) })
  }

  def numericLength(s: ShapeParserState): Parser[NumericFacet] = {
    (ignorecase("TOTALDIGITS") ~> integer ^^ { case n => TotalDigits(n) }
      | ignorecase("FRACTIONDIGITS") ~> integer ^^ { case n => FractionDigits(n) })
  }

  def xsFacet(s: ShapeParserState): Parser[XSFacet] = {
    (numericFacet(s)
      | stringFacet(s))
  }

  def predicate(s: ShapeParserState): Parser[IRI] =
    (iri(s.namespaces)
      | symbol("a") ^^^ rdf_type)

  def repeatCount: Parser[Cardinality] = {
    (symbol("*") ^^^ star
      | symbol("+") ^^^ plus
      | symbol("?") ^^^ optional
      | symbol("{") ~> integer ~ upperBound <~ symbol("}") ^^
      {
        case m ~ maybeN =>
          (m, maybeN) match {
            case (m, None) => RangeCardinality(m, m)
            case (m, Some(None)) => UnboundedCardinalityFrom(m)
            case (m, Some(Some(n))) => {
              // TODO: Add error checking here? (m > n...)
              RangeCardinality(m, n)
            }
          }
      })
  }
  
  // Some(None) = Unbounded
  // None = Unspecified
  // Some(Some(n)) = Bounded by N
  def upperBound: Parser[Option[Option[Int]]] = {
    ( opt(symbol(",") ~> opt(integer | symbol("*"))) ^^ {
      case None => None
      case Some(None) => Some(None)
      case Some(Some(n: Integer)) => Some(Some(n))
      case Some(Some(_)) => Some(None)
    })
  }
  
  
  // [0]     startActions          ::= codeDecl+
  def startActions: StateParser[ShapeParserState, Actions] = {  
    semanticActions
  }

  
  def semanticActions: StateParser[ShapeParserState, Actions] = { s =>  
    repS(action)(s) ^^ { case (as,s1) => (Actions.fromList(as),s1)}
  }
  
  def action: StateParser[ShapeParserState, (IRI, String)] = { s => 
    symbol("%") ~> opt(WS) ~> opt(iri(s.namespaces)) ~ codeDecl ^^ {
      case (None ~ str) => ((IRI(""), str), s)
      case (Some(iri) ~ str) => ((iri, str), s)
    }
  }

  def codeDecl: Parser[String] = {
    opt(WS) ~> "{" ~> code <~ symbol("%") <~ symbol("}") 
  }
  
  def code: Parser[String] = {
   acceptRegex("Code Decl", ("""([^%\\]|\\[%\\]|""" + UCHAR_STR + """)*""").r) ^^ {
     case str => unscapeUchars((unscapePercent(str)))
   }
  }
   
def unscapePercent(s: String): String = {
    @tailrec
    def unscapeHelper(s: LinearSeq[Char], tmp: StringBuilder): String = {
      s match {
        case '\\' :: '\\' :: rs =>
          unscapeHelper(rs, tmp ++= "\\" )
        case '\\' :: '%' :: rs =>
          unscapeHelper(rs, tmp ++= "%")
        case c :: rs => unscapeHelper(rs, tmp += c)
        case Nil => tmp.mkString
      }
    }
    unscapeHelper(s.toList, new StringBuilder)
}

 def dot = opt(WS) ~> symbol(".") <~ opt(WS)

  def integer: Parser[Int] = {
    """\d\d*""".r ^^ (s => s.toInt)
  }

  def regexChars: Parser[String] = {
    ( STRING_LITERAL_SINGLE_QUOTE 
    | STRING_LITERAL_LONG_QUOTE
    | STRING_LITERAL_QUOTE
    )
  }
  
  // Parsing symbols skipping spaces...
  // TODO: should refactor to other file 
  def symbol(str: Parser[String]): Parser[String] = {
    opt(WS) ~> str <~ opt(WS)
  }

  // The following trick to parse ignoring cases was taken from: 
  // http://stackoverflow.com/questions/6080437/case-insensitive-scala-parser-combinator
  def ignorecase(str: String): Parser[String] = {
    opt(WS) ~> ("""(?i)\Q""" + str + """\E""").r <~ opt(WS)
  }

  // TODO: Move this method to StateParser
  def lift[S,A](p:Parser[A]): StateParser[S,A] = { s =>
    p ^^ { case v => (v,s) } 
  }
  
  def iriBase(pm: PrefixMap, base: IRI): Parser[IRI] = {
    iri(pm) ^^ { case iri => base.resolve(iri) }
  }

}

object ShaclParser extends ShaclParser {

  /**
   * Parse a string with a base IRI
   * @param s: input string
   * @param baseIRI: Initial Base IRI
   * @return Left(rs) = list of shapes successfully parsed
   *         Right(msg) = Error msg
   */
  def parse(s: CharSequence, baseIRI: IRI = IRI("")): Try[(Schema, PrefixMap)] = {
    try {
      val state = ShapeParserState.initial.newBase(baseIRI)
      parseAll(shexDoc(state), new CharSequenceReader(s)) match {
        case Success((schema, s), in1) =>
          UtilSuccess((schema, s.namespaces))
        case Error(msg, in1) =>
          UtilFailure(new Exception("Error at " + in1.pos + ": " + msg))
        case Failure(msg, in1) => {
          UtilFailure(new Exception("Failure at " + in1.pos + ": " + msg))
        }
      }
    } catch {
      case e: Exception => scala.util.Failure(e)
    }
  }

}