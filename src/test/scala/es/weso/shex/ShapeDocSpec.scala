package es.weso.shex

import es.weso.shex.ShapeSyntax._
import es.weso.shex.ShapeDoc._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph._
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.Checkers
import es.weso.parser.PrefixMap

class ShapeDocSpec 
	extends FunSpec 
	with Matchers 
	with Checkers {

 describe("Shape Doc") {
   it("Should be able to define basic mbox") {
     val termFoafmbox = NameTerm(t = IRI(foaf + "mbox"))
     val arcRule = ArcRule(n = termFoafmbox,
                           v = typeShexIRI,
                           id = None)
     val shape = Shape(IRILabel(IRI("test")),arcRule)
     val sd = ShapeDoc(pm = PrefixMap.empty)
     info("Shape: " + sd.shape2String(shape))
  }

  it("Should be able to define basic UserShapes") {
     val termFoaf_mbox = NameTerm(t = IRI(foaf + "mbox"))
     val termFoaf_name = NameTerm(t = IRI(foaf + "name"))
     val termFoaf_givenName = NameTerm(t = IRI(foaf + "givenName"))
     val termFoaf_familyName = NameTerm(t = IRI(foaf + "familyName"))
     
     val nameRule = ArcRule(n = termFoaf_name,
                           v = typeXsdString,
                           id = None)
                           
     val givenNameRule = ArcRule(n = termFoaf_givenName,
                           v = typeXsdString,
                           id = None)
                           
     val familyNameRule = ArcRule(n = termFoaf_familyName,
                           v = typeXsdString,
                           id = None)
                           
     val givenName_and_familyName = AndRule(r1 = givenNameRule,r2 = familyNameRule)                      
     
     val or_names = OrRule(r1 = nameRule,r2 = givenName_and_familyName)
                           
     val mboxRule = ArcRule(n = termFoaf_mbox,
                           v = typeShexIRI,
                           id = None)
 
     val shape = Shape(IRILabel(IRI("test")),AndRule(or_names,mboxRule))
     val pm = PrefixMaps.commonShex
     val schema = Schema(pm = pm, shEx = ShEx(rules = Seq(shape), start = None))
     info("Schema: " + schema.toString)
  }

  
 }   
    
  
}