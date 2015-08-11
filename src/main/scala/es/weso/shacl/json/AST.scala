package es.weso.shacl.json
import argonaut._
import Argonaut._
import argonaut.DecodeJsons

object AST {
  
case class SchemaAST(
      prefixes: Option[Map[String,String]],
      shapes: Option[Map[String,ShapeAST]],
      startAct: Option[Map[String,String]],
      start: Option[String]
)

object SchemaAST {
  def empty = SchemaAST(prefixes = None, shapes=None, startAct=None,start=None)
}

implicit def SchemaEncodeJson: EncodeJson[SchemaAST] =
    EncodeJson((n: SchemaAST) =>
      ("prefixes" := n.prefixes.asJson) ->:
      ("shapes" := n.shapes.asJson) ->:
      ("startAct" := n.startAct.asJson) ->:
      ("start" := n.start.asJson) ->:
       jEmptyObject
      )

def obj2Json(obj: Map[String,String]): Json = {
  obj.foldRight(jEmptyObject){ case ((alias,uri),rest) => {
    (alias := uri) ->: rest
  }} 
}

def shapes2Json(shapes: Map[String,ShapeAST]): Json = {
  shapes.foldRight(jEmptyObject){ case ((label,shape),rest) => {
    (label := shape.asJson) ->: rest
  }} 
}

implicit def SchemaDecodeJson: DecodeJson[SchemaAST] =
    DecodeJson((c) => for {
      prefixes <- (c --\ "prefixes").as[Option[Map[String,String]]] 
      shapes <- (c --\ "shapes").as[Option[Map[String,ShapeAST]]]
      startAct <- (c --\ "startAct").as[Option[Map[String,String]]]
      start <- (c --\ "start").as[Option[String]]
    } yield SchemaAST(prefixes,shapes,startAct,start)
    )
      
case class ShapeAST(
      expression: Option[ExpressionAST],
      closed: Option[Boolean],
      inherit: Option[List[String]]
  )
  
object ShapeAST {
  def empty = ShapeAST(None,None,None)
}

implicit def ShapeEncodeJson: EncodeJson[ShapeAST] =
    EncodeJson((n: ShapeAST) =>
      ("type" := jString("shape")) ->:
       ("expression" := n.expression.asJson) ->:
       ("closed" := n.closed.asJson) ->:
       ("inherit" := n.inherit.asJson) ->:
       jEmptyObject
      )

implicit def ShapeDecodeJson: DecodeJson[ShapeAST] = {
  DecodeJson((c) => for {
    expression <- (c --\ "expression").as[Option[ExpressionAST]]
    closed <- (c --\ "closed").as[Option[Boolean]]
    inherit <- (c --\ "inherit").as[Option[List[String]]]
  } yield ShapeAST(expression,closed,inherit))
}
  
  
case class ExpressionAST(
    _type: String,
    id: Option[String],
    predicate: String,
    include: Option[String],
    value: ValueClassAST,
    values: Option[List[Either[String,StemAST]]],
    inverse: Option[Boolean],
    negated: Option[Boolean],
    min: Option[Int],
    max: Option[Int],
    expressions: Option[List[ExpressionAST]],
    annotations:Option[List[List[String]]],
    semAct:Option[Map[String,String]]
    )
    
object ExpressionAST {
  def empty = 
    ExpressionAST(
        _type="",
        id = None,
        predicate="",
        include=None,
        value=ValueClassAST.empty,
        values=None,
        inverse=None,negated=None,
        min=None,max=None,
        expressions=None,
        annotations=None,
        semAct=None
    )
}
    
implicit def ExpressionEncodeJson: EncodeJson[ExpressionAST] =
    EncodeJson((n: ExpressionAST) =>
      ("type" := n._type) ->:
      ("id" := n.id.asJson) ->:
      ("predicate" := n.predicate) ->: 
      ("include" := n.include) ->: 
      ("value" := n.value.asJson) ->: 
      ("values" := n.values.asJson) ->: 
      ("inverse" := n.inverse.asJson) ->:
      ("negated" := n.negated.asJson) ->:
      ("min" := n.min.asJson) ->:
      ("max" := n.max.asJson) ->:
      ("expressions" := n.expressions.asJson) ->:
      ("annotations" := n.annotations.asJson) ->:
      ("semAct" := n.semAct.asJson) ->:
      jEmptyObject)

implicit def ExpressionDecodeJson: DecodeJson[ExpressionAST] =
    DecodeJson((c) => for {
     _type <- (c --\ "type").as[String]
     id <- (c --\ "id").as[Option[String]]
     predicate <- (c --\ "predicate").as[String]
     include <- (c --\ "include").as[Option[String]]
     value <- (c --\ "value").as[ValueClassAST]
     values <- (c --\ "values").as[Option[List[Either[String,StemAST]]]]
     inverse <- (c --\ "inverse").as[Option[Boolean]]
     negated <- (c --\ "negated").as[Option[Boolean]]
     min <- (c --\ "min").as[Option[Int]]
     max <- (c --\ "max").as[Option[Int]]
     expressions <- (c --\ "expressions").as[Option[List[ExpressionAST]]]
     annotations <- (c --\ "annotations").as[Option[List[List[String]]]]
     semAct <- (c --\ "semAct").as[Option[Map[String,String]]]
    } yield ExpressionAST(_type,id,predicate,include,value,values,inverse,negated,min,max,expressions,annotations,semAct))


/* The following declaration would be nice but generates stack overflow...
  implicit def ExpressionASTCodecJson: CodecJson[ExpressionAST] =
    casecodec9(ExpressionAST.apply, ExpressionAST.unapply)(
        "type", 
        "predicate", 
        "value",
        "values",
        "inverse",
        "negated",
        "min",
        "max",
        "expressions"
    ) */

case class ValueClassAST(
    nodeKind: Option[String],
    pattern: Option[String],
    reference: Option[ReferenceAST],
    length: Option[Int],
    minInclusive: Option[Int],
    maxInclusive: Option[Int],
    minExclusive: Option[Int],
    maxExclusive: Option[Int]
) 

object ValueClassAST {
  lazy val empty = ValueClassAST(
      nodeKind=None, 
      pattern=None,
      reference=None,
      length=None,
      minInclusive=None,
      maxInclusive=None,
      minExclusive=None,
      maxExclusive=None)
}

implicit def ValueClassEncodeJson: EncodeJson[ValueClassAST] =
    EncodeJson((n: ValueClassAST) =>
      ("type" := jString("valueClass")) ->:
      ("pattern" := n.pattern.asJson) ->: 
      ("nodeKind" := n.nodeKind.asJson) ->: 
      ("reference" := n.reference.asJson) ->: 
      ("length" := n.length) ->: 
      ("minInclusive" := n.minInclusive) ->: 
      ("maxInclusive" := n.maxInclusive) ->: 
      ("minExclusive" := n.minExclusive) ->: 
      ("maxExclusive" := n.maxExclusive) ->: 
      jEmptyObject)

    
implicit def ValueClassDecodeJson: DecodeJson[ValueClassAST] =
    DecodeJson((c) => for {
     length <- (c --\ "length").as[Option[Int]]
     pattern <- (c --\ "pattern").as[Option[String]]
     reference <- (c --\ "reference").as[Option[ReferenceAST]]
     nodeKind <- (c --\ "nodeKind").as[Option[String]]
     minInclusive <- (c --\ "mininclusive").as[Option[Int]]
     maxInclusive <- (c --\ "maxinclusive").as[Option[Int]]
     minExclusive <- (c --\ "minexclusive").as[Option[Int]]
     maxExclusive <- (c --\ "maxexclusive").as[Option[Int]]
    } yield ValueClassAST(
        nodeKind,
        pattern,
        reference,
        length,
        minInclusive,maxInclusive,
        minExclusive,maxExclusive)
    )

case class ReferenceAST(
    value: Either[String,OrAST]
) 

object ReferenceAST {
  lazy val empty = Left("")
}

implicit def ReferenceEncodeJson: EncodeJson[ReferenceAST] =
    EncodeJson((n: ReferenceAST) =>
      n.value.asJson)

    
implicit def ReferenceDecodeJson: DecodeJson[ReferenceAST] =
    DecodeJson((c) => 
      (for { value <- c.as[String] } yield ReferenceAST(Left(value))) |||
      (for { value <- c.as[OrAST] } yield ReferenceAST(Right(value)))
      )      
    
        
case class OrAST(
    disjuncts: List[String]
) 

object OrAST {
  lazy val empty = OrAST(disjuncts=List())
}

implicit def OrEncodeJson: EncodeJson[OrAST] =
    EncodeJson((n: OrAST) =>
      ("type" := jString("or")) ->:
      ("disjuncts" := n.disjuncts.asJson) ->: 
      jEmptyObject
      )

    
implicit def OrDecodeJson: DecodeJson[OrAST] =
    DecodeJson((c) => for {
     disjuncts <- (c --\ "disjuncts").as[List[String]]
    } yield OrAST(disjuncts))      

case class StemRangeAST(
    stem: Either[String,StemAST],
    exclusions: Option[List[StemAST]]
) 

object StemRangeAST {
  lazy val empty = 
    StemRangeAST(
        stem=Left(""), 
        exclusions=None)
}

implicit def StemRangeEncodeJson: EncodeJson[StemRangeAST] =
    EncodeJson((n: StemRangeAST) =>
      ("type" := jString("stemRange")) ->:
      ("stem" := n.stem.asJson) ->: 
      ("exclusions" := n.exclusions.asJson) ->: 
      jEmptyObject
      )

    
implicit def StemRangeDecodeJson: DecodeJson[StemRangeAST] =
    DecodeJson((c) => for {
     stem <- (c --\ "stem").as[Either[String,StemAST]]
     exclusions <- (c --\ "exclusions").as[Option[List[StemAST]]]
    } yield StemRangeAST(stem,exclusions))      
    
case class StemAST(
    _type: Option[String]
) 

object StemAST {
  lazy val empty = 
    StemAST(None)
}

implicit def StemEncodeJson: EncodeJson[StemAST] =
    EncodeJson((n: StemAST) =>
      ("type" := n._type.asJson) ->:
      jEmptyObject
      )

    
implicit def StemDecodeJson: DecodeJson[StemAST] =
    DecodeJson((c) => for {
     _type <- (c --\ "type").as[Option[String]]
    } yield StemAST(_type))      

}
