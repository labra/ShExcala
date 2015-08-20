package es.weso.shacl.json
import argonaut._
import Argonaut._
import argonaut.DecodeJsons

object AST {
  
case class SchemaAST(
      prefixes: Option[Map[String,String]],
      shapes: Option[Map[String,ShapeAST]],
      start: Option[String],
      startActions: Option[Map[String,String]]
)

case class ShapeAST(
      expression: Option[ExpressionAST],
      virtual: Option[Boolean],
      closed: Option[Boolean],
      inherit: Option[List[String]],
      extra: Option[List[String]],
      semAct: Option[Map[String,String]]
  )

case class ExpressionAST(
    _type: String,
    id: Option[String],
    predicate: Option[String],
    include: Option[String],
    value: Option[ValueClassAST],
    inverse: Option[Boolean],
    negated: Option[Boolean],
    min: Option[Int],
    max: Option[Int],
    expressions: Option[List[ExpressionAST]],
    annotations:Option[List[List[String]]],
    semAct:Option[Map[String,String]]
    )

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
    datatype: Option[String]
) 

case class ValueAST(
   value: Either[String, StemRangeAST]
)

case class ReferenceAST(
    value: Either[String,OrAST]
) 

case class OrAST(
    disjuncts: List[String]
) 

case class StemRangeAST(
    _type: Option[String],
    stem: Option[StemAST],
    exclusions: Option[List[ExclusionAST]]
) 

case class ExclusionAST(
    value: Either[String, StemAST]
) 

case class StemAST(
    value: Either[String, WildCardAST]
)

case class WildCardAST(
    _type: Option[String],
    stem: Option[StemAST]
)



// Empty initializers
// There should be a better way to do this    
object SchemaAST {
  def empty = SchemaAST(prefixes = None, 
      shapes=None, 
      start=None,
      startActions=None
      )
}
object ShapeAST {
  def empty = ShapeAST(None,None,None,None,None,None)
}

object ExpressionAST {
  def empty = 
    ExpressionAST(
        _type="",
        id = None,
        predicate=None,
        include=None,
        value=None,
        inverse=None,
        negated=None,
        min=None,
        max=None,
        expressions=None,
        annotations=None,
        semAct=None
    )
}

object ValueClassAST {
  lazy val empty = ValueClassAST(
      values=None,
      nodeKind=None, 
      pattern=None,
      reference=None,
      length=None,
      minInclusive=None,
      maxInclusive=None,
      minExclusive=None,
      maxExclusive=None,
      minLength=None,
      maxLength=None,
      totalDigits=None,
      fractionDigits=None,
      datatype=None)
}

object ReferenceAST {
  lazy val empty = Left("")
}

object OrAST {
  lazy val empty = OrAST(disjuncts=List())
}

object StemRangeAST {
  lazy val empty = StemRangeAST(_type=None,stem=None,exclusions=None)
}

object ExclusionAST {
  lazy val empty = ExclusionAST(Left(""))
}

object StemAST {
  lazy val empty = StemAST(Left(""))
}

// JSON Encoders
implicit def SchemaEncodeJson: EncodeJson[SchemaAST] =
    EncodeJson((n: SchemaAST) =>
      ("prefixes" :=? n.prefixes) ->?:
      ("shapes" :=? n.shapes) ->?:
      ("start" :=? n.start) ->?:
      ("startAct" :=? n.startActions) ->?:
       jEmptyObject
      )

implicit def ShapeEncodeJson: EncodeJson[ShapeAST] =
    EncodeJson((n: ShapeAST) =>
      ("type" := jString("shape")) ->:
       ("expression" :=? n.expression) ->?:
       ("virtual" :=? n.virtual) ->?:
       ("closed" :=? n.closed) ->?:
       ("inherit" :=? n.inherit) ->?:
       ("extra" :=? n.extra) ->?:
       ("semAct" :=? n.semAct) ->?:
       jEmptyObject
      )

implicit def ExpressionEncodeJson: EncodeJson[ExpressionAST] =
    EncodeJson((n: ExpressionAST) =>
      ("type" := n._type) ->:
      ("id" :=? n.id) ->?:
      ("predicate" :=? n.predicate) ->?: 
      ("include" :=? n.include) ->?: 
      ("value" :=? n.value) ->?: 
      ("inverse" :=? n.inverse) ->?:
      ("negated" :=? n.negated) ->?:
      ("min" :=? n.min) ->?:
      ("max" :=? n.max) ->?:
      ("expressions" :=? n.expressions) ->?:
      ("annotations" :=? n.annotations) ->?:
      ("semAct" :=? n.semAct) ->?:
      jEmptyObject)
      
implicit def ValueClassEncodeJson: EncodeJson[ValueClassAST] =
    EncodeJson((n: ValueClassAST) =>
      ("type" := jString("valueClass")) ->:
      ("values" :=? n.values) ->?:
      ("pattern" :=? n.pattern) ->?: 
      ("nodeKind" :=? n.nodeKind) ->?: 
      ("reference" :=? n.reference) ->?: 
      ("length" :=? n.length) ->?: 
      ("minInclusive" :=? n.minInclusive) ->?: 
      ("maxInclusive" :=? n.maxInclusive) ->?: 
      ("minExclusive" :=? n.minExclusive) ->?: 
      ("maxExclusive" :=? n.maxExclusive) ->?: 
      ("minLength" :=? n.minLength) ->?: 
      ("maxLength" :=? n.maxLength) ->?: 
      ("totalDigits" :=? n.totalDigits) ->?: 
      ("fractionDigits" :=? n.fractionDigits) ->?: 
      ("datatype" :=? n.datatype) ->?: 
      jEmptyObject)

implicit def ValueEncodeJson: EncodeJson[ValueAST] =
    EncodeJson((n: ValueAST) =>
      n.value.asJson
    )
      
implicit def ReferenceEncodeJson: EncodeJson[ReferenceAST] =
    EncodeJson((n: ReferenceAST) =>
      n.value.asJson)

implicit def OrEncodeJson: EncodeJson[OrAST] =
    EncodeJson((n: OrAST) =>
      ("type" := jString("or")) ->:
      ("disjuncts" := n.disjuncts) ->: 
      jEmptyObject
      )
      

implicit def StemRangeEncodeJson: EncodeJson[StemRangeAST] =
    EncodeJson((n: StemRangeAST) =>
      ("type" := jString("stemRange")) ->:
      ("stem" := n.stem.asJson) ->: 
      ("exclusions" := n.exclusions.asJson) ->: 
      jEmptyObject
      )

implicit def ExclusionEncodeJson: EncodeJson[ExclusionAST] =
    EncodeJson((n: ExclusionAST) =>
      n.value.asJson
      )
      
implicit def StemEncodeJson: EncodeJson[StemAST] =
    EncodeJson((n: StemAST) =>
      n.value.asJson
      )

implicit def WildCardEncodeJson: EncodeJson[WildCardAST] =
    EncodeJson((n: WildCardAST) =>
      ("type" := jString("wildcard")) ->:
      jEmptyObject
      )
// Json decoders
implicit def SchemaDecodeJson: DecodeJson[SchemaAST] =
    DecodeJson((c) => for {
      prefixes <- (c --\ "prefixes").as[Option[Map[String,String]]] 
      shapes <- (c --\ "shapes").as[Option[Map[String,ShapeAST]]]
      start <- (c --\ "start").as[Option[String]]
      startActions <- (c --\ "startAct").as[Option[Map[String,String]]]
    } yield SchemaAST(prefixes,shapes,start, startActions)
    )

implicit def ShapeDecodeJson: DecodeJson[ShapeAST] = {
  DecodeJson((c) => for {
    expression <- (c --\ "expression").as[Option[ExpressionAST]]
    virtual <- (c --\ "virtual").as[Option[Boolean]]
    closed <- (c --\ "closed").as[Option[Boolean]]
    inherit <- (c --\ "inherit").as[Option[List[String]]]
    extra <- (c --\ "extra").as[Option[List[String]]]
    semAct <- (c --\ "semAct").as[Option[Map[String,String]]]
  } yield ShapeAST(expression,virtual,closed,inherit,extra,semAct))
}
  
implicit def ExpressionDecodeJson: DecodeJson[ExpressionAST] =
    DecodeJson((c) => for {
     _type <- (c --\ "type").as[String]
     id <- (c --\ "id").as[Option[String]]
     predicate <- (c --\ "predicate").as[Option[String]]
     include <- (c --\ "include").as[Option[String]]
     value <- (c --\ "value").as[Option[ValueClassAST]]
     inverse <- (c --\ "inverse").as[Option[Boolean]]
     negated <- (c --\ "negated").as[Option[Boolean]]
     min <- (c --\ "min").as[Option[Int]]
     max <- (c --\ "max").as[Option[Int]]
     expressions <- (c --\ "expressions").as[Option[List[ExpressionAST]]]
     annotations <- (c --\ "annotations").as[Option[List[List[String]]]]
     semAct <- (c --\ "semAct").as[Option[Map[String,String]]]
    } yield ExpressionAST(_type,id,predicate,include,value,inverse,negated,min,max,expressions,annotations,semAct))


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
        minInclusive,maxInclusive,
        minExclusive,maxExclusive,
        minlength,maxlength,
        totaldigits,fractiondigits,
        datatype)
    )

implicit def ValueDecodeJson: DecodeJson[ValueAST] =
    DecodeJson((c) => 
      (for { value <- c.as[String] } yield ValueAST(Left(value))) |||
      (for { value <- c.as[StemRangeAST] } yield ValueAST(Right(value)))
      )      
 
implicit def ReferenceDecodeJson: DecodeJson[ReferenceAST] =
    DecodeJson((c) => 
      (for { value <- c.as[String] } yield ReferenceAST(Left(value))) |||
      (for { value <- c.as[OrAST] } yield ReferenceAST(Right(value)))
      )      
       
  
implicit def OrDecodeJson: DecodeJson[OrAST] =
    DecodeJson((c) => for {
     disjuncts <- (c --\ "disjuncts").as[List[String]]
    } yield OrAST(disjuncts))      

   
implicit def StemRangeDecodeJson: DecodeJson[StemRangeAST] =
    DecodeJson((c) => for {
     _type <- (c --\ "type").as[Option[String]]
     stem <- (c --\ "stem").as[Option[StemAST]]
     exclusions <- (c --\ "exclusions").as[Option[List[ExclusionAST]]]
    } yield StemRangeAST(_type=_type, stem= stem,exclusions=exclusions))      

implicit def ExclusionDecodeJson: DecodeJson[ExclusionAST] =
    DecodeJson((c) => 
      (for { value <- c.as[String] } yield ExclusionAST(Left(value))) |||
      (for { value <- c.as[StemAST] } yield ExclusionAST(Right(value)))
      )      
    
implicit def StemDecodeJson: DecodeJson[StemAST] =
    DecodeJson((c) => 
      (for { value <- c.as[String] } yield StemAST(Left(value))) |||
      (for { value <- c.as[WildCardAST] } yield StemAST(Right(value)))
    )      

implicit def WildCardDecodeJson: DecodeJson[WildCardAST] =
    DecodeJson((c) => for {
     _type <- (c --\ "type").as[Option[String]]
     stem <- (c --\ "stem").as[Option[StemAST]]
    } yield WildCardAST(_type,stem))      
    
}
