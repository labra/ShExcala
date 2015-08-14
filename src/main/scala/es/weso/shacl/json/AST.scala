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
      closed: Option[Boolean],
      inherit: Option[List[String]]
  )

case class ExpressionAST(
    _type: String,
    id: Option[String],
    predicate: String,
    include: Option[String],
    value: ValueClassAST,
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
    fractionDigits: Option[Int]
) 

case class ValueAST(
   value: Either[String,StemAST]
)

case class ReferenceAST(
    value: Either[String,OrAST]
) 

case class OrAST(
    disjuncts: List[String]
) 

case class StemRangeAST(
    stem: Either[String,StemAST],
    exclusions: Option[List[StemAST]]
) 

case class StemAST(
    _type: Option[String]
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
  def empty = ShapeAST(None,None,None)
}

object ExpressionAST {
  def empty = 
    ExpressionAST(
        _type="",
        id = None,
        predicate="",
        include=None,
        value=ValueClassAST.empty,
        inverse=None,negated=None,
        min=None,max=None,
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
      fractionDigits=None)
}

object ReferenceAST {
  lazy val empty = Left("")
}

object OrAST {
  lazy val empty = OrAST(disjuncts=List())
}

object StemRangeAST {
  lazy val empty = StemRangeAST(stem=Left(""),exclusions=None)
}

object StemAST {
  lazy val empty = StemAST(None)
}

// JSON Encoders
implicit def SchemaEncodeJson: EncodeJson[SchemaAST] =
    EncodeJson((n: SchemaAST) =>
      ("prefixes" := n.prefixes.asJson) ->:
      ("shapes" := n.shapes.asJson) ->:
      ("start" := n.start.asJson) ->:
      ("startAct" := n.startActions.asJson) ->:
       jEmptyObject
      )

implicit def ShapeEncodeJson: EncodeJson[ShapeAST] =
    EncodeJson((n: ShapeAST) =>
      ("type" := jString("shape")) ->:
       ("expression" := n.expression.asJson) ->:
       ("closed" := n.closed.asJson) ->:
       ("inherit" := n.inherit.asJson) ->:
       jEmptyObject
      )

implicit def ExpressionEncodeJson: EncodeJson[ExpressionAST] =
    EncodeJson((n: ExpressionAST) =>
      ("type" := n._type) ->:
      ("id" := n.id.asJson) ->:
      ("predicate" := n.predicate) ->: 
      ("include" := n.include) ->: 
      ("value" := n.value.asJson) ->: 
      ("inverse" := n.inverse.asJson) ->:
      ("negated" := n.negated.asJson) ->:
      ("min" := n.min.asJson) ->:
      ("max" := n.max.asJson) ->:
      ("expressions" := n.expressions.asJson) ->:
      ("annotations" := n.annotations.asJson) ->:
      ("semAct" := n.semAct.asJson) ->:
      jEmptyObject)
      
implicit def ValueClassEncodeJson: EncodeJson[ValueClassAST] =
    EncodeJson((n: ValueClassAST) =>
      ("type" := jString("valueClass")) ->:
      ("values" := n.values.asJson) ->:
      ("pattern" := n.pattern.asJson) ->: 
      ("nodeKind" := n.nodeKind.asJson) ->: 
      ("reference" := n.reference.asJson) ->: 
      ("length" := n.length) ->: 
      ("minInclusive" := n.minInclusive) ->: 
      ("maxInclusive" := n.maxInclusive) ->: 
      ("minExclusive" := n.minExclusive) ->: 
      ("maxExclusive" := n.maxExclusive) ->: 
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
      ("disjuncts" := n.disjuncts.asJson) ->: 
      jEmptyObject
      )
      

implicit def StemRangeEncodeJson: EncodeJson[StemRangeAST] =
    EncodeJson((n: StemRangeAST) =>
      ("type" := jString("stemRange")) ->:
      ("stem" := n.stem.asJson) ->: 
      ("exclusions" := n.exclusions.asJson) ->: 
      jEmptyObject
      )

implicit def StemEncodeJson: EncodeJson[StemAST] =
    EncodeJson((n: StemAST) =>
      ("type" := n._type.asJson) ->:
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
    closed <- (c --\ "closed").as[Option[Boolean]]
    inherit <- (c --\ "inherit").as[Option[List[String]]]
  } yield ShapeAST(expression,closed,inherit))
}
  
implicit def ExpressionDecodeJson: DecodeJson[ExpressionAST] =
    DecodeJson((c) => for {
     _type <- (c --\ "type").as[String]
     id <- (c --\ "id").as[Option[String]]
     predicate <- (c --\ "predicate").as[String]
     include <- (c --\ "include").as[Option[String]]
     value <- (c --\ "value").as[ValueClassAST]
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
    } yield ValueClassAST(
        values,
        nodeKind,
        pattern,
        reference,
        length,
        minInclusive,maxInclusive,
        minExclusive,maxExclusive,
        minlength,maxlength,
        totaldigits,fractiondigits)
    )

implicit def ValueDecodeJson: DecodeJson[ValueAST] =
    DecodeJson((c) => 
      (for { value <- c.as[String] } yield ValueAST(Left(value))) |||
      (for { value <- c.as[StemAST] } yield ValueAST(Right(value)))
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
     stem <- (c --\ "stem").as[Either[String,StemAST]]
     exclusions <- (c --\ "exclusions").as[Option[List[StemAST]]]
    } yield StemRangeAST(stem,exclusions))      
   
implicit def StemDecodeJson: DecodeJson[StemAST] =
    DecodeJson((c) => for {
     _type <- (c --\ "type").as[Option[String]]
    } yield StemAST(_type))      

}
