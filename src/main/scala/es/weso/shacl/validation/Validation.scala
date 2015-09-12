package es.weso.shacl.validation
import argonaut._
import Argonaut._
import argonaut.DecodeJsons

object Validation {
  
case class ValAST(
    _type: String,
    node: String,
    shape: String,
    solution: Option[SolutionAST]
)

case class SolutionAST(
    _type: String,
    predicate: Option[String],
    value: Option[ValueClassAST],
    solutions: Option[List[TestedTriple]],
    min: Option[Int],
    max: Option[Either[Int,String]]  // It can be a number or a star
)

case class TestedTriple(
    subject: String,
    predicate: String,
    _object: String 
    )

case class ValueClassAST(
) 

object ShapeAST {
  def empty = ValAST("", "","",None)
}

object SolutionAST {
  def empty = 
    SolutionAST(
        _type="",
        predicate = None,
        value = None,
        solutions = None,
        min = None,
        max = None
    )
}

object ValueClassAST {
  lazy val empty = ValueClassAST()
}

implicit def ShapeEncodeJson: EncodeJson[ValAST] =
    EncodeJson((n: ValAST) =>
      ("type" := n._type) ->:
      ("node" := n.node) ->:
      ("shape" := n.shape) ->:
      ("solution" :=? n.solution) ->?:
       jEmptyObject
      )

implicit def ExpressionEncodeJson: EncodeJson[SolutionAST] =
    EncodeJson((n: SolutionAST) =>
      ("type" := n._type) ->:
      ("predicate" :=? n.predicate) ->?:
      ("value" :=? n.value) ->?:
      ("solutions" :=? n.solutions) ->?:
      ("max" :=? n.max) ->?:
      ("min" :=? n.min) ->?:
      jEmptyObject)
      
implicit def ValueClassEncodeJson: EncodeJson[ValueClassAST] =
    EncodeJson((n: ValueClassAST) =>
      ("type" := jString("valueClass")) ->:
      jEmptyObject)

implicit def TestedTripleJson: EncodeJson[TestedTriple] =
    EncodeJson((n: TestedTriple) =>
      ("type" := jString("testedTriple")) ->:
      ("subject" := n.subject) ->:
      ("predicate" := n.predicate) ->:
      ("object" := n._object) ->:
      jEmptyObject)
      
 implicit def ValASTDecodeJson: DecodeJson[ValAST] = {
  DecodeJson((c) => for {
    _type <- (c --\ "type").as[String]
    node <- (c --\ "node").as[String]
    shape <- (c --\ "shape").as[String]
    solution <- (c --\ "solution").as[Option[SolutionAST]]
  } yield ValAST(_type,node,shape,solution))
}
  
implicit def SolutionDecodeJson: DecodeJson[SolutionAST] =
    DecodeJson((c) => 
      for {
     _type <- (c --\ "type").as[String]
     predicate <- (c --\ "predicate").as[Option[String]]
     value <- (c --\ "value").as[Option[ValueClassAST]]
     solutions <- (c --\ "solutions").as[Option[List[TestedTriple]]]
     min <- (c --\ "min").as[Option[Int]]
     max <- (c --\ "max").as[Option[Either[Int,String]]]
    } yield SolutionAST(
        _type,predicate,value,solutions,min,max)
   )


implicit def ValueClassDecodeJson: DecodeJson[ValueClassAST] =
    DecodeJson((c) => for {
      _type <- (c --\ "type").as[String]
    } yield ValueClassAST()
   )
    
implicit def MaxDecodeJson: DecodeJson[Either[Int,String]] =
    DecodeJson((c) => 
      (for { value <- c.as[String] } yield Right(value)) |||
      (for { value <- c.as[Int] } yield Left(value))
      )      
  

implicit def TestedTripleDecodeJson: DecodeJson[TestedTriple] =
    DecodeJson((c) => for {
     _type <- (c --\ "type").as[String]
     subject <- (c --\ "subject").as[String]
     predicate <- (c --\ "predicate").as[String]
     _object <- (c --\ "object").as[String]
    } yield TestedTriple(
        subject= subject,predicate=predicate, _object = _object))      
    
}
