package es.weso.shacl.validation
import argonaut._
import Argonaut._
import argonaut.DecodeJsons
import es.weso.shacl.jast._

trait Validation extends AST {
  
  case class Validation(value: Option[ValAST])

  case class ValAST(
    _type: String,
    node: String,
    shape: String,
    solution: Option[ExpressionAST],
    solutions: Option[List[ExpressionAST]], // This one could be removed later
    startActs: Option[List[SemActAST]]
    )
    
  object ValAST {
    def empty = ValAST("", "", "", None,None,None)
  }


  implicit def ValidationEncode: EncodeJson[Validation] =
    EncodeJson((n: Validation) =>
       n.value match {
         case None => jNull
         case Some(valAST) => valAST.asJson
       }
    )
      
  implicit def ValASTEncodeJson: EncodeJson[ValAST] =
    EncodeJson((n: ValAST) =>
      ("type" := n._type) ->:
        ("node" := n.node) ->:
        ("shape" := n.shape) ->:
        ("solution" :=? n.solution) ->?:
        ("solutions" :=? n.solutions) ->?:
        ("startActs" :=? n.startActs) ->?:
        jEmptyObject)


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
      solutions <- (c --\ "solutions").as[Option[List[ExpressionAST]]]
      startActs <- (c --\ "startActs").as[Option[List[SemActAST]]]
    } yield ValAST(_type, node, shape, solution,solutions,startActs))
  }


}

