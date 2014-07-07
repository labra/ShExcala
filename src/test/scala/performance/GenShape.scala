package es.weso.performance

import es.weso.shex.Schema
import es.weso.shex.ShapeSyntax._
import es.weso.rdfgraph.nodes._
import es.weso.shex.PrefixMaps
import es.weso.rdf.RDFTriples
import es.weso.rdfgraph.statements.RDFTriple


object GenShape {
  val prefix = "http://example.org/"
    
  def iri(str: String) = IRI(prefix + str)
  
  def genAnds(n:Int): Schema = {
    

    def ruleAN(n:Int) : Rule = {
      ArcRule(id = None, n  = NameTerm(iri("a")),
                         v  = ValueSet(Seq(RDFNodeObject(IntegerLiteral(n)))))
    } 
    
    def ands(n:Int) : Rule = {
      def mkAnd(r:Rule, n:Int): Rule = AndRule(r,ruleAN(n))
      (1 to n).toList.foldLeft(EmptyRule: Rule)(mkAnd)
    }

    val labelA = IRILabel(iri("a"))
    val shape : Shape = Shape(label = labelA, rule = ands(n) )
    val shex : ShEx = ShEx( rules=List(shape), start=None )
    Schema(shEx = shex, pm = PrefixMaps.example)
  }

  def genTriples(n:Int): RDFTriples = {
    
    val ts : Set[RDFTriple] =
      (for (v <- 1 to n) 
       yield RDFTriple(iri("x"),iri("a"),IntegerLiteral(v))).toSet
    RDFTriples(ts,PrefixMaps.example)
  } 
}