package es.weso.shacl.jast
import argonaut._
import Argonaut._
import argonaut.DecodeJsons

object AST {

  case class SchemaAST(
    prefixes: Option[Map[String, String]],
    valueClasses: Option[Map[String,ValueClassAST]],
    shapes: Option[Map[String, ShapeAST]],
    start: Option[String],
    startActions: Option[Seq[ActionAST]])

  case class ShapeAST(
    expression: Option[ExpressionAST],
    virtual: Option[Boolean],
    closed: Option[Boolean],
    inherit: Option[Seq[String]],
    extra: Option[Seq[String]],
    semAct: Option[Seq[ActionAST]])

  case class ExpressionAST(
    _type: String,
//    id: Option[String],
    predicate: Option[String],
    include: Option[String],
    valueExpr: Option[ValueClassAST],
    inverse: Option[Boolean],
    negated: Option[Boolean],
    min: Option[Int],
    max: Option[MaxAST], // It can be a number or a star    expressions: Option[List[ExpressionAST]],
    expression: Option[ExpressionAST],
    expressions: Option[List[ExpressionAST]],
    annotations: Option[List[List[String]]],
    semAct: Option[Seq[ActionAST]],
    valueClassRef: Option[String]
    )

  case class ValueClassAST(
    values: Option[List[ValueAST]],
    nodeKind: Option[String],
    pattern: Option[String],
    reference: Option[ReferenceAST],
    length: Option[Int],
    minInclusive: Option[NumberAST],
    maxInclusive: Option[NumberAST],
    minExclusive: Option[NumberAST],
    maxExclusive: Option[NumberAST],
    minLength: Option[Int],
    maxLength: Option[Int],
    totalDigits: Option[Int],
    fractionDigits: Option[Int],
    datatype: Option[String])

  case class MaxAST(val v: Option[Int]) {
  }
  
  case class NumberAST(val v: Either[Int,Double]) {
  }


  case class ValueAST(
    value: Either[String, StemRangeAST])

  case class ReferenceAST(
    value: Either[String, AndAST])

  case class AndAST(
    conjuncts: List[String])

  case class StemRangeAST(
//    _type: Option[String],
    stem: StemAST,
    exclusions: Option[List[ExclusionAST]])

  case class ExclusionAST(
    value: Either[String, StemAST])

  case class StemAST(value: Either[String, WildCard])
  
  case class WildCard(_type: String)

  case class ActionAST(
    name: String, 
    contents: String
  )
  
  // Empty initializers
  // There should be a better way to do this    
  object SchemaAST {
    def empty = SchemaAST(prefixes = None,
      valueClasses = None,
      shapes = None,
      start = None,
      startActions = None)
  }
  object ShapeAST {
    def empty = ShapeAST(None, None, None, None, None, None)
  }

  object ExpressionAST {
    def empty =
      ExpressionAST(
        _type = "",
//        id = None,
        predicate = None,
        include = None,
        valueExpr = None,
        inverse = None,
        negated = None,
        min = None,
        max = None,
        expression = None,
        expressions = None,
        annotations = None,
        semAct = None,
        valueClassRef = None
        )
  }

  object MaxAST {
    def empty = MaxAST(None)
  }

  object ValueClassAST {
    lazy val empty = ValueClassAST(
      values = None,
      nodeKind = None,
      pattern = None,
      reference = None,
      length = None,
      minInclusive = None,
      maxInclusive = None,
      minExclusive = None,
      maxExclusive = None,
      minLength = None,
      maxLength = None,
      totalDigits = None,
      fractionDigits = None,
      datatype = None)
  }

  object ReferenceAST {
    lazy val empty = Left("")
  }

  object AndAST {
    lazy val empty = AndAST(conjuncts = List())
  }

/*  object StemRangeAST {
    lazy val empty = StemRangeAST(
        stem = Right(WildCard("wildcard")), 
        exclusions = None)
  } */

  object ExclusionAST {
    lazy val empty = ExclusionAST(Left(""))
  }

  object StemAST {
    lazy val empty = StemAST(Left(""))
  }
  
  object WildCard {
    lazy val empty = WildCard(_type = "wildcard")
  }
  
  // JSON Encoders
  implicit def SchemaEncodeJson: EncodeJson[SchemaAST] =
    EncodeJson((n: SchemaAST) =>
      ("startActs" :=? n.startActions) ->?:
        ("start" :=? n.start) ->?:
        ("valueClasses" :=? n.valueClasses) ->?:
        ("shapes" :=? n.shapes) ->?:
        ("prefixes" := n.prefixes.getOrElse(Map())) ->:
        ("type" := jString("schema")) ->:
        jEmptyObject)

  implicit def ShapeEncodeJson: EncodeJson[ShapeAST] =
    EncodeJson((n: ShapeAST) =>
      ("semActs" :=? n.semAct) ->?:
        ("extra" :=? n.extra) ->?:
        ("inherit" :=? n.inherit) ->?:
        ("closed" :=? n.closed) ->?:
        ("virtual" :=? n.virtual) ->?:
        ("expression" :=? n.expression) ->?:
        ("type" := jString("shape")) ->:
        jEmptyObject)

  implicit def ExpressionEncodeJson: EncodeJson[ExpressionAST] =
    EncodeJson((n: ExpressionAST) =>
      ("semActs" :=? n.semAct) ->?:
        ("annotations" :=? n.annotations) ->?:
        ("expression" :=? n.expression) ->?:
        ("expressions" :=? n.expressions) ->?:
        ("max" :=? n.max) ->?:
        ("min" :=? n.min) ->?:
        ("negated" :=? n.negated) ->?:
        ("inverse" :=? n.inverse) ->?:
        ("value" :=? n.valueExpr) ->?:
        ("include" :=? n.include) ->?:
        ("predicate" :=? n.predicate) ->?:
//        ("id" :=? n.id) ->?:
        ("valueClassRef" :=? n.valueClassRef) ->?:
        ("type" := n._type) ->:
        jEmptyObject)

  implicit def MaxEncodeJson: EncodeJson[MaxAST] =
    EncodeJson((n: MaxAST) =>
      n.v match {
        case None    => jString("*")
        case Some(x) => jNumber(x)
      })

  implicit def NumberEncodeJson: EncodeJson[NumberAST] =
    EncodeJson((n: NumberAST) =>
      n.v match {
        case Left(x)  => jNumber(x)
        case Right(x) => jNumber(x)
      })
      
  implicit def ValueClassEncodeJson: EncodeJson[ValueClassAST] =
    EncodeJson((n: ValueClassAST) =>
      ("mininclusive" :=? n.minInclusive) ->?:
        ("maxinclusive" :=? n.maxInclusive) ->?:
        ("minexclusive" :=? n.minExclusive) ->?:
        ("maxexclusive" :=? n.maxExclusive) ->?:
        ("minlength" :=? n.minLength) ->?:
        ("maxlength" :=? n.maxLength) ->?:
        ("totaldigits" :=? n.totalDigits) ->?:
        ("fractiondigits" :=? n.fractionDigits) ->?:
        ("length" :=? n.length) ->?:
        ("reference" :=? n.reference) ->?:
        ("datatype" :=? n.datatype) ->?:
        ("nodeKind" :=? n.nodeKind) ->?:
        ("pattern" :=? n.pattern) ->?:
        ("values" :=? n.values) ->?:
        ("type" := jString("valueClass")) ->:
        jEmptyObject)

  implicit def ValueEncodeJson: EncodeJson[ValueAST] =
    EncodeJson((n: ValueAST) =>
      n.value.fold(_.asJson, _.asJson))

  implicit def ReferenceEncodeJson: EncodeJson[ReferenceAST] =
    EncodeJson((n: ReferenceAST) =>
      n.value.fold(_.asJson, _.asJson))

  implicit def OrEncodeJson: EncodeJson[AndAST] =
    EncodeJson((n: AndAST) =>
      ("conjuncts" := n.conjuncts) ->:
        ("type" := jString("and")) ->:
        jEmptyObject)

  implicit def StemRangeEncodeJson: EncodeJson[StemRangeAST] =
    EncodeJson((n: StemRangeAST) =>
      ("exclusions" :=? n.exclusions) ->?:
        ("stem" := n.stem) ->:
        ("type" := jString("stemRange")) ->:
        jEmptyObject)

  implicit def ExclusionEncodeJson: EncodeJson[ExclusionAST] =
    EncodeJson((n: ExclusionAST) =>
      n.value.fold(_.asJson,(stem) => 
        ("stem" := stem.asJson) ->:
        ("type" := jString("stem")) ->:
        jEmptyObject)
    )

  implicit def StemEncodeJson: EncodeJson[StemAST] =
    EncodeJson((n: StemAST) =>
        n.value.fold(_.asJson, _.asJson)
    )

/*  implicit def StringStemEncodeJson: EncodeJson[StringStemAST] =
    EncodeJson((n: StringStemAST) =>
      ("stem" := n.value) ->:
        jEmptyObject) */
        
  implicit def WildCardEncodeJson: EncodeJson[WildCard] =
    EncodeJson((n: WildCard) =>
      ("type" := jString("wildcard")) ->:
        jEmptyObject)
        
  implicit def ActionEncodeJson: EncodeJson[ActionAST] =
    EncodeJson((n: ActionAST) =>
      ("name" := n.name) ->:
      ("contents" := n.contents) ->:
        jEmptyObject)
        
  // Json decoders
  implicit def SchemaDecodeJson: DecodeJson[SchemaAST] =
    DecodeJson((c) => for {
      prefixes <- (c --\ "prefixes").as[Option[Map[String, String]]]
      valueClasses <- (c --\ "valueClasses").as[Option[Map[String, ValueClassAST]]]
      shapes <- (c --\ "shapes").as[Option[Map[String, ShapeAST]]]
      start <- (c --\ "start").as[Option[String]]
      startActions <- (c --\ "startActs").as[Option[Seq[ActionAST]]]
    } yield SchemaAST(prefixes, valueClasses, shapes, start, startActions))

  implicit def ShapeDecodeJson: DecodeJson[ShapeAST] = {
    DecodeJson((c) => for {
      expression <- (c --\ "expression").as[Option[ExpressionAST]]
      virtual <- (c --\ "virtual").as[Option[Boolean]]
      closed <- (c --\ "closed").as[Option[Boolean]]
      inherit <- (c --\ "inherit").as[Option[List[String]]]
      extra <- (c --\ "extra").as[Option[List[String]]]
      semAct <- (c --\ "semActs").as[Option[Seq[ActionAST]]]
    } yield ShapeAST(expression, virtual, closed, inherit, extra, semAct))
  }

  implicit def ExpressionDecodeJson: DecodeJson[ExpressionAST] =
    DecodeJson((c) => for {
      _type <- (c --\ "type").as[String]
//      id <- (c --\ "id").as[Option[String]]
      predicate <- (c --\ "predicate").as[Option[String]]
      include <- (c --\ "include").as[Option[String]]
      valueExpr <- (c --\ "valueExpr").as[Option[ValueClassAST]]
      inverse <- (c --\ "inverse").as[Option[Boolean]]
      negated <- (c --\ "negated").as[Option[Boolean]]
      min <- (c --\ "min").as[Option[Int]]
      max <- (c --\ "max").as[Option[MaxAST]]
      expression <- (c --\ "expression").as[Option[ExpressionAST]]
      expressions <- (c --\ "expressions").as[Option[List[ExpressionAST]]]
      annotations <- (c --\ "annotations").as[Option[List[List[String]]]]
      semAct <- (c --\ "semActs").as[Option[Seq[ActionAST]]]
      valueClassRef <- (c --\ "valueClassRef").as[Option[String]]
    } yield ExpressionAST(
        _type = _type, 
        predicate, include, valueExpr, inverse, negated, min, max, 
        expression, expressions, annotations, semAct, valueClassRef))

  /* The following declaration would be nice but generates stack overflow...
  implicit def ExpressionASTCodecJson: CodecJson[ExpressionAST] =
    casecodec9(ExpressionAST.apply, ExpressionAST.unapply)(
        "type","predicate","value","values","inverse","negated","min","max","expressions"
    ) */

  implicit def ValueClassDecodeJson: DecodeJson[ValueClassAST] =
    DecodeJson((c) => for {
      values <- (c --\ "values").as[Option[List[ValueAST]]]
      length <- (c --\ "length").as[Option[Int]]
      pattern <- (c --\ "pattern").as[Option[String]]
      reference <- (c --\ "reference").as[Option[ReferenceAST]]
      nodeKind <- (c --\ "nodeKind").as[Option[String]]
      minInclusive <- (c --\ "mininclusive").as[Option[NumberAST]]
      maxInclusive <- (c --\ "maxinclusive").as[Option[NumberAST]]
      minExclusive <- (c --\ "minexclusive").as[Option[NumberAST]]
      maxExclusive <- (c --\ "maxexclusive").as[Option[NumberAST]]
      minlength <- (c --\ "minlength").as[Option[Int]]
      maxlength <- (c --\ "maxlength").as[Option[Int]]
      totaldigits <- (c --\ "totaldigits").as[Option[Int]]
      fractiondigits <- (c --\ "fractiondigits").as[Option[Int]]
      datatype <- (c --\ "datatype").as[Option[String]]
    } yield ValueClassAST(
      values,
      nodeKind,
      pattern,
      reference,
      length,
      minInclusive, maxInclusive,
      minExclusive, maxExclusive,
      minlength, maxlength,
      totaldigits, fractiondigits,
      datatype))

  implicit def MaxDecodeJson: DecodeJson[MaxAST] =
    DecodeJson((c) =>
      // TODO: Check that the string is a *
      (for { value <- c.as[String] } yield MaxAST(None)) |||
        (for { value <- c.as[Int] } yield MaxAST(Some(value))))

  implicit def NumberDecodeJson: DecodeJson[NumberAST] =
    DecodeJson((c) =>
      // TODO: Check that the string is a *
      (for { value <- c.as[Int] } yield NumberAST(Left(value))) |||
      (for { value <- c.as[Double] } yield NumberAST(Right(value))))
        
  implicit def ValueDecodeJson: DecodeJson[ValueAST] =
    DecodeJson((c) =>
      (for { value <- c.as[String] } yield ValueAST(Left(value))) |||
        (for { value <- c.as[StemRangeAST] } yield ValueAST(Right(value))))

  implicit def ReferenceDecodeJson: DecodeJson[ReferenceAST] =
    DecodeJson((c) =>
      (for { value <- c.as[String] } yield ReferenceAST(Left(value))) |||
        (for { value <- c.as[AndAST] } yield ReferenceAST(Right(value))))

  implicit def OrDecodeJson: DecodeJson[AndAST] =
    DecodeJson((c) => for {
      conjuncts <- (c --\ "conjuncts").as[List[String]]
    } yield AndAST(conjuncts))

  implicit def StemRangeDecodeJson: DecodeJson[StemRangeAST] =
    DecodeJson((c) => for {
//      _type <- (c --\ "type").as[Option[String]]
      stem <- (c --\ "stem").as[StemAST]
      exclusions <- (c --\ "exclusions").as[Option[List[ExclusionAST]]]
    } yield StemRangeAST(stem = stem, exclusions = exclusions))

  implicit def ExclusionDecodeJson: DecodeJson[ExclusionAST] =
    DecodeJson((c) =>
      (for { value <- c.as[String] } yield ExclusionAST(Left(value))) |||
      (for { value <- (c --\ "stem").as[StemAST] } yield ExclusionAST(Right(value))))

  implicit def StemDecodeJson: DecodeJson[StemAST] =
    DecodeJson((c) =>
      (for { value <- c.as[String] 
      } yield StemAST(Left(value))) |||
      (for { value <- c.as[WildCard] 
      } yield StemAST(Right(value)))
    )

  implicit def WildCardDecodeJson: DecodeJson[WildCard] =
    DecodeJson((c) => for {
//      _type <- (c --\ "type").as[Option[String]]
//      if (_type == "wildcard") 
      _type <- (c --\ "type").as[String]  // Check that the value is wildcard
//      exclusions <- (c --\ "exclusions").as[List[ExclusionAST]]
    } yield WildCard(_type))

/*  implicit def StringStemDecodeJson: DecodeJson[StringStemAST] =
    DecodeJson((c) => if (c. for {
//      _type <- (c --\ "type").as[Option[String]]
//      if (_type == "wildcard") 
      _type <- (c --\ "type").as[Option[String]]  // Check that the value is wildcard
      exclusions <- (c --\ "exvlusions").as[List[ExclusionAST]]
    } yield StringStemAST(exclusions)) */
    
  implicit def ActionDecodeJson: DecodeJson[ActionAST] =
    DecodeJson((c) => for {
      name <- (c --\ "name").as[String]
      contents <- (c --\ "contents").as[String]
    } yield ActionAST(name, contents))
}
