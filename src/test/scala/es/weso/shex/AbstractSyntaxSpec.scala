package es.weso.shex

import es.weso.shex.AbstractSyntax._
import es.weso.shex.PrettyPrint._
import es.weso.rdfNode._
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers

class AbstractSyntaxSpec 
	extends FunSpec 
	with Matchers 
	with Checkers {

 describe("Abstract syntax") {
   it("Should be able to define basic mbox") {
     val termFoafmbox = NameTerm(t = IRI(foaf + "mbox"))
     val arcRule = ArcRule(n = termFoafmbox,
                           v = typeShexIRI,
                           c = Default,
                           a = NoActions,
                           id = NoId)
     val shape = Shape(IRILabel(IRI("test")),arcRule)
     info("Shape: " + prettyPrint(shape))
  }

  it("Should be able to define basic UserShapes") {
     val termFoaf_mbox = NameTerm(t = IRI(foaf + "mbox"))
     val termFoaf_name = NameTerm(t = IRI(foaf + "name"))
     val termFoaf_givenName = NameTerm(t = IRI(foaf + "givenName"))
     val termFoaf_familyName = NameTerm(t = IRI(foaf + "givenName"))
     
     val nameRule = ArcRule(n = termFoaf_name,
                           v = typeXsdString,
                           c = Default,
                           a = NoActions,
                           id = NoId)
                           
     val givenNameRule = ArcRule(n = termFoaf_givenName,
                           v = typeXsdString,
                           c = Plus,
                           a = NoActions,
                           id = NoId)
                           
     val familyNameRule = ArcRule(n = termFoaf_familyName,
                           v = typeXsdString,
                           c = Default,
                           a = NoActions,
                           id = NoId)
                           
     val givenName_and_familyName = AndRule(conjoints=Seq(givenNameRule,familyNameRule))                      
     
     val or_names = GroupRule(OrRule(disjoints=Seq(nameRule,givenName_and_familyName)), opt=false, a = NoActions)
                           
     val mboxRule = ArcRule(n = termFoaf_mbox,
                           v = typeShexIRI,
                           c = Default,
                           a = NoActions,
                           id = NoId)
 
     val shape = Shape(IRILabel(IRI("test")),AndRule(conjoints = Seq(or_names,mboxRule)))
     info("Shape: " + prettyPrint(shape))
  }

 }   
    
  
}