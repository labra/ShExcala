package es.weso.shacl.validation
import argonaut._
import Argonaut._
import argonaut.DecodeJsons

object Validation {
  
  case class Validation(value: Option[ValAST])

  case class ValAST(
    _type: String,
    node: String,
    shape: String,
    solution: Option[ExpressionAST])

  case class ExpressionAST(
    _type: Option[String],
    value: Option[ValueClassAST],
    solutions: Option[List[ExpressionAST]],
    min: Option[Int],
    max: Option[MaxAST], // It can be a number or a star
    subject: Option[String],
    predicate: Option[String],
    _object: Option[String],
    expressions: Option[List[ExpressionAST]],
    referenced: Option[ValAST],
    annotations: Option[List[List[String]]],
    semAct: Option[Map[String, String]])

  case class ValueClassAST(
    values: Option[List[ValueAST]],
    nodeKind: Option[String],
    pattern: Option[String],
    reference: Option[ReferenceAST],
    length: Option[Int],
    minInclusive: Option[Int],
    maxInclusive: Option[Int],
    minExclusive: Option[Int],
    maxExclusive: Option[Int],
    minLength: Option[Int],
    maxLength: Option[Int],
    totalDigits: Option[Int],
    fractionDigits: Option[Int],
    datatype: Option[String])

  case class MaxAST(val v: Option[Int]) {
  }

  case class ValueAST(
    value: Either[String, StemRangeAST])

  case class ReferenceAST(
    value: Either[String, OrAST])

  case class OrAST(
    disjuncts: List[String])

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

  object ShapeAST {
    def empty = ValAST("", "", "", None)
  }

  object ExpressionAST {
    def empty =
      ExpressionAST(
        _type = None,
        value = None,
        solutions = None,
        min = None,
        max = None,
        subject = None,
        predicate = None,
        _object = None,
        expressions = None,
        referenced = None,
        annotations = None,
        semAct = None)
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

  object OrAST {
    lazy val empty = OrAST(disjuncts = List())
  }

  object StemRangeAST {
    lazy val empty = StemRangeAST(_type = None, stem = None, exclusions = None)
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

  implicit def ValidationEncode: EncodeJson[Validation] =
    EncodeJson((n: Validation) =>
       n.value match {
         case None => jNull
         case Some(valAST) => valAST.asJson
       }
    )
      
  implicit def ShapeEncodeJson: EncodeJson[ValAST] =
    EncodeJson((n: ValAST) =>
      ("type" := n._type) ->:
        ("node" := n.node) ->:
        ("shape" := n.shape) ->:
        ("solution" :=? n.solution) ->?:
        jEmptyObject)

  implicit def ExpressionEncodeJson: EncodeJson[ExpressionAST] =
    EncodeJson((n: ExpressionAST) =>
      ("type" :=? n._type) ->?:
        ("value" :=? n.value) ->?:
        ("solutions" :=? n.solutions) ->?:
        ("min" :=? n.min) ->?:
        ("max" :=? n.max) ->?:
        ("subject" :=? n.subject) ->?:
        ("predicate" :=? n.predicate) ->?:
        ("object" :=? n._object) ->?:
        ("expressions" :=? n.expressions) ->?:
        ("referenced" :=? n.referenced) ->?:
        ("semAct" :=? n.semAct) ->?:
        ("annotations" :=? n.annotations) ->?:
        jEmptyObject)

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

  implicit def OrEncodeJson: EncodeJson[OrAST] =
    EncodeJson((n: OrAST) =>
      ("disjuncts" := n.disjuncts) ->:
        ("type" := jString("or")) ->:
        jEmptyObject)

  implicit def StemRangeEncodeJson: EncodeJson[StemRangeAST] =
    EncodeJson((n: StemRangeAST) =>
      ("exclusions" :=? n.exclusions) ->?:
        ("stem" :=? n.stem) ->?:
        ("type" := jString("stemRange")) ->:
        jEmptyObject)

  implicit def ExclusionEncodeJson: EncodeJson[ExclusionAST] =
    EncodeJson((n: ExclusionAST) =>
      n.value.asJson)

  implicit def StemEncodeJson: EncodeJson[StemAST] =
    EncodeJson((n: StemAST) =>
      n.value.asJson)

  implicit def WildCardEncodeJson: EncodeJson[WildCardAST] =
    EncodeJson((n: WildCardAST) =>
      ("type" := jString("wildcard")) ->:
        jEmptyObject)

  implicit def MaxEncodeJson: EncodeJson[MaxAST] =
    EncodeJson((n: MaxAST) =>
      n.v match {
        case None    => jString("*")
        case Some(x) => jNumber(x)
      })

  // Decoders
      
implicit def ValidationDecodeJson: DecodeJson[Validation] = {
    DecodeJson((c) =>
       for { valAST <- c.as[Option[ValAST]] 
       } yield Validation(valAST)
       ) 
  }
      

  implicit def ValASTDecodeJson: DecodeJson[ValAST] = {
    DecodeJson((c) => for {
      _type <- (c --\ "type").as[String]
      node <- (c --\ "node").as[String]
      shape <- (c --\ "shape").as[String]
      solution <- (c --\ "solution").as[Option[ExpressionAST]]
    } yield ValAST(_type, node, shape, solution))
  }

  implicit def ExpressionDecodeJson: DecodeJson[ExpressionAST] =
    DecodeJson((c) =>
      for {
        _type <- (c --\ "type").as[Option[String]]
        value <- (c --\ "value").as[Option[ValueClassAST]]
        solutions <- (c --\ "solutions").as[Option[List[ExpressionAST]]]
        min <- (c --\ "min").as[Option[Int]]
        max <- (c --\ "max").as[Option[MaxAST]]
        subject <- (c --\ "subject").as[Option[String]]
        predicate <- (c --\ "predicate").as[Option[String]]
        _object <- (c --\ "object").as[Option[String]]
        expressions <- (c --\ "expressions").as[Option[List[ExpressionAST]]]
        referenced <- (c --\ "referenced").as[Option[ValAST]]
        annotations <- (c --\ "annotations").as[Option[List[List[String]]]]
        semAct <- (c --\ "semAct").as[Option[Map[String, String]]]
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
        semAct = semAct))

  implicit def ValueClassDecodeJson: DecodeJson[ValueClassAST] =
    DecodeJson((c) => for {
      values <- (c --\ "values").as[Option[List[ValueAST]]]
      length <- (c --\ "length").as[Option[Int]]
      pattern <- (c --\ "pattern").as[Option[String]]
      reference <- (c --\ "reference").as[Option[ReferenceAST]]
      nodeKind <- (c --\ "nodeKind").as[Option[String]]
      minInclusive <- (c --\ "mininclusive").as[Option[Int]]
      maxInclusive <- (c --\ "maxinclusive").as[Option[Int]]
      minExclusive <- (c --\ "minexclusive").as[Option[Int]]
      maxExclusive <- (c --\ "maxexclusive").as[Option[Int]]
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

  implicit def ValueDecodeJson: DecodeJson[ValueAST] =
    DecodeJson((c) =>
      (for { value <- c.as[String] } yield ValueAST(Left(value))) |||
        (for { value <- c.as[StemRangeAST] } yield ValueAST(Right(value))))

  implicit def ReferenceDecodeJson: DecodeJson[ReferenceAST] =
    DecodeJson((c) =>
      (for { value <- c.as[String] } yield ReferenceAST(Left(value))) |||
        (for { value <- c.as[OrAST] } yield ReferenceAST(Right(value))))

  implicit def OrDecodeJson: DecodeJson[OrAST] =
    DecodeJson((c) => for {
      disjuncts <- (c --\ "disjuncts").as[List[String]]
    } yield OrAST(disjuncts))

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

}

