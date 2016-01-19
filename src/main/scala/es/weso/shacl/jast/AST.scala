package es.weso.shacl.jast
import argonaut._
import Argonaut.{IntDecodeJson => _, _ }
import argonaut.DecodeJsons

trait AST {

  case class SchemaAST(
    prefixes: Option[Map[String, String]],
    valueClasses: Option[Map[String, ValueClassAST]],
    shapes: Option[Map[String, ShapeAST]],
    start: Option[String],
    startActions: Option[Seq[SemActAST]])

  case class ShapeAST(
    expression: Option[ExpressionAST],
    virtual: Option[Boolean],
    closed: Option[Boolean],
    inherit: Option[Seq[String]],
    extra: Option[Seq[String]],
    semAct: Option[Seq[SemActAST]])

  case class ExpressionAST(
    _type: Option[String],
    value: Option[ValueClassAST],
    solutions: Option[List[ExpressionAST]],
    inverse: Option[Boolean],
    negated: Option[Boolean],    
    min: Option[Int],
    max: Option[MaxAST], // It can be a number or a star
    subject: Option[String],
    predicate: Option[String],
    _object: Option[String],
    expressions: Option[List[ExpressionAST]],
    referenced: Option[ValueAST],
    annotations: Option[List[AnnotationAST]],
    //    semAct: Option[Map[String, String]],
    semActs: Option[List[SemActAST]],
    valueExpr: Option[ValueClassAST])

  case class SemActAST(
    _type: Option[String],
    name: Option[String],
    code: Option[String])

  case class AnnotationAST(
    _type: Option[String],
    predicate: Option[String],
    _object: Option[String])

  case class ValueClassAST(
    _type: Option[String],
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
    datatype: Option[String],
    valueExprRef: Option[String],
    valueExprs: Option[List[ValueClassAST]])

  case class MaxAST(val v: Option[Int]) {
  }

  case class NumberAST(val v: Either[Int, Double]) {
  }

  case class ValueAST(
    value: Either[String, StemRangeAST])

  case class ReferenceAST(
    value: Either[String, Either[OrAST,AndAST]])

  case class OrAST(
    disjuncts: List[String])
    
  case class AndAST(
    conjuncts: List[String])  

  case class StemRangeAST(
    _type: Option[String],
    stem: Option[StemAST],
    exclusions: Option[List[ExclusionAST]])

  case class ExclusionAST(
    value: Either[String, StemAST])

  case class StemAST(
    value: Either[String, WildCardAST])

  case class WildCardAST(
    _type: Option[String],
    stem: Option[StemAST])

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
        _type = None,
        value = None,
        solutions = None,
        inverse = None,
        negated = None,
        min = None,
        max = None,
        subject = None,
        predicate = None,
        _object = None,
        expressions = None,
        referenced = None,
        annotations = None,
        //        semAct = None,
        semActs = None,
        valueExpr = None)
  }

  object ValueClassAST {
    lazy val empty = ValueClassAST(
      _type = Some("ValueClass"),
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
      datatype = None,
      valueExprRef = None,
      valueExprs = None)
  }

  /*  object ValueExprAST {
    lazy val empty = ValueExprAST(None,None)
  } */

  object SemActAST {
    lazy val empty = SemActAST(None, None, None)
  }

  object AnnotationAST {
    lazy val empty = AnnotationAST(None, None, None)
  }

  object ReferenceAST {
    lazy val empty = Left("")
  }

  object OrAST {
    lazy val empty = OrAST(disjuncts = List())
  }

  object StemRangeAST {
    lazy val empty = StemRangeAST(
      _type = None,
      stem = None,
      exclusions = None)
  }

  object ExclusionAST {
    lazy val empty = ExclusionAST(Left(""))
  }

  object StemAST {
    lazy val empty = StemAST(Left(""))
  }

  object MaxAST {
    def empty = MaxAST(None)
  }
  
  object WildCardAST {
    lazy val empty = 
      WildCardAST(
          _type = Some("Wildcard"),
          stem = None
          )
  }

  // JSON Encoders
  implicit def SchemaEncodeJson: EncodeJson[SchemaAST] =
    EncodeJson((n: SchemaAST) =>
      ("startActs" :=? n.startActions) ->?:
        ("start" :=? n.start) ->?:
        ("valueClasses" :=? n.valueClasses) ->?:
        ("shapes" :=? n.shapes) ->?:
        ("prefixes" := n.prefixes.getOrElse(Map())) ->:
        ("type" := jString("Schema")) ->:
        jEmptyObject)

  implicit def ShapeEncodeJson: EncodeJson[ShapeAST] =
    EncodeJson((n: ShapeAST) =>
      ("semActs" :=? n.semAct) ->?:
        ("extra" :=? n.extra) ->?:
        ("inherit" :=? n.inherit) ->?:
        ("closed" :=? n.closed) ->?:
        ("virtual" :=? n.virtual) ->?:
        ("expression" :=? n.expression) ->?:
        ("type" := jString("Shape")) ->:
        jEmptyObject)

  implicit def ExpressionEncodeJson: EncodeJson[ExpressionAST] =
    EncodeJson((n: ExpressionAST) =>
      ("type" :=? n._type) ->?:
        ("value" :=? n.value) ->?:
        ("solutions" :=? n.solutions) ->?:
        ("inverse" :=? n.inverse) ->?:
        ("negated" :=? n.negated) ->?:
        ("min" :=? n.min) ->?:
        ("max" :=? n.max) ->?:
        ("subject" :=? n.subject) ->?:
        ("predicate" :=? n.predicate) ->?:
        ("object" :=? n._object) ->?:
        ("expressions" :=? n.expressions) ->?:
        ("referenced" :=? n.referenced) ->?:
        ("annotations" :=? n.annotations) ->?:
        ("valueExpr" :=? n.valueExpr) ->?:
        //        ("semAct" :=? n.semAct) ->?:
        ("semActs" :=? n.semActs) ->?:
        jEmptyObject)

  /*  implicit def ValueExprEncodeJson: EncodeJson[ValueExprAST] =
    EncodeJson((n: ValueExprAST) =>
      ("type" :=? n._type) ->?:
        ("valueExprRef" :=? n.valueExprRef) ->?:
        jEmptyObject) */

  implicit def SemActEncodeJson: EncodeJson[SemActAST] =
    EncodeJson((n: SemActAST) =>
      ("type" :=? n._type) ->?:
        ("name" :=? n.name) ->?:
        ("code" :=? n.code) ->?:
        jEmptyObject)

  implicit def AnnotationJson: EncodeJson[AnnotationAST] =
    EncodeJson((n: AnnotationAST) =>
      ("type" :=? n._type) ->?:
        ("predicate" :=? n.predicate) ->?:
        ("object" :=? n._object) ->?:
        jEmptyObject)

  implicit def ValueClassEncodeJson: EncodeJson[ValueClassAST] =
    EncodeJson((n: ValueClassAST) =>
      ("type" :=? n._type) ->?:
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
        ("valueExprRef" :=? n.valueExprRef) ->?:
        ("valueExprs" :=? n.valueExprs) ->?:
        jEmptyObject)

  implicit def ValueEncodeJson: EncodeJson[ValueAST] =
    EncodeJson((n: ValueAST) =>
      n.value.fold(_.asJson, _.asJson))

  implicit def ReferenceEncodeJson: EncodeJson[ReferenceAST] =
    EncodeJson((n: ReferenceAST) =>
      n.value.fold(_.asJson, (e: Either[OrAST,AndAST]) => 
        e.fold(_.asJson, _.asJson)
      ))

  implicit def OrEncodeJson: EncodeJson[OrAST] =
    EncodeJson((n: OrAST) =>
      ("disjuncts" := n.disjuncts) ->:
        ("type" := jString("or")) ->:
        jEmptyObject)

  implicit def AndEncodeJson: EncodeJson[AndAST] =
    EncodeJson((n: AndAST) =>
      ("conjuncts" := n.conjuncts) ->:
        ("type" := jString("and")) ->:
        jEmptyObject)
        
  implicit def StemRangeEncodeJson: EncodeJson[StemRangeAST] =
    EncodeJson((n: StemRangeAST) =>
      ("exclusions" :=? n.exclusions) ->?:
        ("stem" :=? n.stem) ->?:
        ("type" := jString("StemRange")) ->:
        jEmptyObject)

  implicit def ExclusionEncodeJson: EncodeJson[ExclusionAST] =
    EncodeJson((n: ExclusionAST) =>
      n.value.fold(str => 
          jString(str)
          , v => 
             ("type" := "Stem") ->: 
             ("stem" := v.asJson) ->:
             jEmptyObject)
          )

  implicit def StemEncodeJson: EncodeJson[StemAST] =
    EncodeJson((n: StemAST) =>
      n.value.fold(str => jString(str), 
          _.asJson)
      )

  implicit def WildCardEncodeJson: EncodeJson[WildCardAST] =
    EncodeJson((n: WildCardAST) =>
      ("type" :=? n._type) ->?:
        ("stem" :=? n.stem) ->?:
        jEmptyObject)

  implicit def NumberEncodeJson: EncodeJson[NumberAST] =
    EncodeJson((n: NumberAST) =>
      n.v match {
        case Left(x)  => jNumber(x)
        case Right(x) => jNumber(x)
      })

  implicit def MaxEncodeJson: EncodeJson[MaxAST] =
    EncodeJson((n: MaxAST) =>
      n.v match {
        case None    => jString("*")
        case Some(x) => jNumber(x)
      })

  // Json decoders
  implicit def SchemaDecodeJson: DecodeJson[SchemaAST] =
    DecodeJson((c) => for {
      prefixes <- (c --\ "prefixes").as[Option[Map[String, String]]]
      valueClasses <- (c --\ "valueClasses").as[Option[Map[String, ValueClassAST]]]
      shapes <- (c --\ "shapes").as[Option[Map[String, ShapeAST]]]
      start <- (c --\ "start").as[Option[String]]
      startActions <- (c --\ "startActs").as[Option[Seq[SemActAST]]]
    } yield SchemaAST(prefixes, valueClasses, shapes, start, startActions))

  implicit def ShapeDecodeJson: DecodeJson[ShapeAST] = {
    DecodeJson((c) => for {
      expression <- (c --\ "expression").as[Option[ExpressionAST]]
      virtual <- (c --\ "virtual").as[Option[Boolean]]
      closed <- (c --\ "closed").as[Option[Boolean]]
      inherit <- (c --\ "inherit").as[Option[List[String]]]
      extra <- (c --\ "extra").as[Option[List[String]]]
      semAct <- (c --\ "semActs").as[Option[Seq[SemActAST]]]
    } yield ShapeAST(expression, virtual, closed, inherit, extra, semAct))
  }

  implicit def ExpressionDecodeJson: DecodeJson[ExpressionAST] =
    DecodeJson((c) =>
      for {
        _type <- (c --\ "type").as[Option[String]]
        value <- (c --\ "value").as[Option[ValueClassAST]]
        solutions <- (c --\ "solutions").as[Option[List[ExpressionAST]]]
        inverse <- (c --\ "inverse").as[Option[Boolean]]
        negated <- (c --\ "negated").as[Option[Boolean]]
        min <- (c --\ "min").as[Option[Int]]
        max <- (c --\ "max").as[Option[MaxAST]]
        subject <- (c --\ "subject").as[Option[String]]
        predicate <- (c --\ "predicate").as[Option[String]]
        _object <- (c --\ "object").as[Option[String]]
        expressions <- (c --\ "expressions").as[Option[List[ExpressionAST]]]
        referenced <- (c --\ "referenced").as[Option[ValueAST]]
        annotations <- (c --\ "annotations").as[Option[List[AnnotationAST]]]
        semAct <- (c --\ "semAct").as[Option[Map[String, String]]]
        semActs <- (c --\ "semActs").as[Option[List[SemActAST]]]
        valueExpr <- (c --\ "valueExpr").as[Option[ValueClassAST]]
      } yield ExpressionAST.empty.copy(
        _type = _type,
        value = value,
        solutions = solutions,
        min = min,
        max = max,
        subject = subject,
        predicate = predicate,
        _object = _object,
        expressions = expressions,
        referenced = referenced,
        annotations = annotations,
        //        semAct = semAct,
        semActs = semActs,
        valueExpr = valueExpr))

  /*  implicit def ValueExprDecodeJson: DecodeJson[ValueExprAST] =
    DecodeJson((c) =>
      for {
        _type <- (c --\ "type").as[Option[String]]
        valueExprRef <- (c --\ "valueExprRef").as[Option[String]]
      } yield ValueExprAST.empty.copy(
        _type = _type,
        valueExprRef = valueExprRef)
      ) */

  implicit def SemActDecodeJson: DecodeJson[SemActAST] =
    DecodeJson((c) =>
      for {
        _type <- (c --\ "type").as[Option[String]]
        name <- (c --\ "name").as[Option[String]]
        code <- (c --\ "code").as[Option[String]]
      } yield SemActAST.empty.copy(
        _type = _type,
        name = name,
        code = code))

  implicit def AnnotationDecodeJson: DecodeJson[AnnotationAST] =
    DecodeJson((c) =>
      for {
        _type <- (c --\ "type").as[Option[String]]
        predicate <- (c --\ "predicate").as[Option[String]]
        _object <- (c --\ "object").as[Option[String]]
      } yield AnnotationAST.empty.copy(
        _type = _type,
        predicate = predicate,
        _object = _object))

  implicit def ValueClassDecodeJson: DecodeJson[ValueClassAST] =
    DecodeJson((c) => for {
      _type <- (c --\ "type").as[Option[String]]
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
      valueExprRef <- (c --\ "valueExprRef").as[Option[String]]
      valueExprs <- (c --\ "valueExprs").as[Option[List[ValueClassAST]]]
    } yield ValueClassAST(
      _type,
      values,
      nodeKind,
      pattern,
      reference,
      length,
      minInclusive, maxInclusive,
      minExclusive, maxExclusive,
      minlength, maxlength,
      totaldigits, fractiondigits,
      datatype,
      valueExprRef,
      valueExprs))

  implicit def ValueDecodeJson: DecodeJson[ValueAST] =
    DecodeJson((c) =>
      (for { value <- c.as[String] } yield ValueAST(Left(value))) |||
        (for { value <- c.as[StemRangeAST] } yield ValueAST(Right(value))))

  implicit def ReferenceDecodeJson: DecodeJson[ReferenceAST] =
    DecodeJson((c) =>
      (for { value <- c.as[String] } yield ReferenceAST(Left(value))) |||
      (for { value <- c.as[OrAST] } yield ReferenceAST(Right(Left(value)))) |||
      (for { value <- c.as[AndAST] } yield ReferenceAST(Right(Right(value))))
      )

  implicit def OrDecodeJson: DecodeJson[OrAST] =
    DecodeJson((c) => for {
      disjuncts <- (c --\ "disjuncts").as[List[String]]
    } yield OrAST(disjuncts))

  implicit def AndDecodeJson: DecodeJson[AndAST] =
    DecodeJson((c) => for {
      conjuncts <- (c --\ "conjuncts").as[List[String]]
    } yield AndAST(conjuncts))
    
  implicit def StemRangeDecodeJson: DecodeJson[StemRangeAST] =
    DecodeJson((c) => for {
      _type <- (c --\ "type").as[Option[String]]
      stem <- (c --\ "stem").as[Option[StemAST]]
      exclusions <- (c --\ "exclusions").as[Option[List[ExclusionAST]]]
    } yield StemRangeAST(_type = _type, stem = stem, exclusions = exclusions))

  implicit def ExclusionDecodeJson: DecodeJson[ExclusionAST] =
    DecodeJson((c) =>
      (for { value <- c.as[String] } yield ExclusionAST(Left(value))) |||
        (for { value <- c.as[StemAST] } yield ExclusionAST(Right(value))))

  implicit def StemDecodeJson: DecodeJson[StemAST] =
    DecodeJson((c) =>
      (for { value <- c.as[String] } yield StemAST(Left(value))) |||
        (for { value <- c.as[WildCardAST] } yield StemAST(Right(value))))

  implicit def WildCardDecodeJson: DecodeJson[WildCardAST] =
    DecodeJson((c) => for {
      _type <- (c --\ "type").as[Option[String]]
      stem <- (c --\ "stem").as[Option[StemAST]]
    } yield WildCardAST(_type, stem))

  implicit def MaxDecodeJson: DecodeJson[MaxAST] =
    DecodeJson((c) =>
      // TODO: Check that the string is a *
      (for { value <- c.as[String] } yield MaxAST(None)) |||
        (for { value <- c.as[Int] } yield MaxAST(Some(value))))

  implicit def NumberDecodeJson: DecodeJson[NumberAST] =
    DecodeJson((c) =>
      (for { value <- c.as[Int] } yield NumberAST(Left(value))) |||
        (for { value <- c.as[Double] } yield NumberAST(Right(value))))

  // The following definition has been adapted from Argonaut's internals
  // It decodes Int only when they are in fact Int's
  // Source: IntDecodeJson in https://github.com/argonaut-io/argonaut/blob/master/argonaut/src/main/scala/argonaut/DecodeJson.scala 
  implicit def decodeStrictInt: DecodeJson[Int] = {
    optionDecoder(x =>
      (x.number flatMap (_.toInt)).orElse(
      (x.string flatMap (s => tryTo(s.toInt)))), "Int")
  }

}

object AST extends AST