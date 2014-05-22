package es.weso.rdf.reader

import com.hp.hpl.jena.query._
import es.weso.rdfgraph.nodes._
import es.weso.rdfgraph.nodes.RDFNode
import es.weso.rdfgraph.statements.RDFTriple
import scala.collection.JavaConversions._
import scala.collection.immutable.StringOps._
import scala.util.Try
import es.weso.rdfgraph.statements._
import com.hp.hpl.jena.rdf.model.{RDFNode => JenaRDFNode}
import com.hp.hpl.jena.rdf.model.Property
import com.hp.hpl.jena.rdf.model.Statement
import com.hp.hpl.jena.rdf.model.Model
import org.slf4j._
import com.hp.hpl.jena.rdf.model.{RDFNode => JenaRDFNode}
import org.apache.jena.riot.RDFDataMgr
import com.hp.hpl.jena.rdf.model.ModelFactory
import es.weso.rdf.RDF

case class RDFFromWeb() extends RDF {
  
  val log = LoggerFactory.getLogger("RDFFromWeb")
  
  lazy val findIRIs = QueryFactory.create(
      """|select ?x where {
         | ?x ?p ?y .
         | filter (isIRI(?x))
	     |}
         |""".stripMargin
      )
      
  lazy val findRDFTriples = QueryFactory.create(
      """|construct { ?x ?p ?y } where {
         | ?x ?p ?y .
	     |}
         |""".stripMargin
      )      
     
  def queryTriples() = {
    QueryFactory.create(
     s"""|construct {?x ?p ?y } where {
         |?x ?p ?y .
	     |}
         |""".stripMargin
    )      
  }

  def queryTriplesWithSubject(subj: IRI) = {
    val s = subj.str 
    QueryFactory.create(
     s"""|construct {<${s}> ?p ?y } where {
         |<${s}> ?p ?y .
	     |}
         |""".stripMargin
    )      
  }
  
  def queryTriplesWithObject(obj: IRI) = {
    val s = obj.str 
    QueryFactory.create(
     s"""|construct {?x ?p <${s}> } where {
         | ?x ?p <${s}> .
	     |}
         |""".stripMargin
    )      
  }

  override def parse(cs: CharSequence): Try[RDF] = {
    throw new Exception("Cannot parse RDFFromWeb " )
  }      

  override def serialize(format:String): String = {
    throw new Exception("Cannot serialize RDFFromWeb")
  }      

  override def iris(): Set[IRI] = {
    throw new Exception("Cannot obtain triples from RDFFromWeb " )
  }  
  
  
  def rdfTriples(): Set[RDFTriple] = {
    throw new Exception("Cannot obtain triples from RDFFromWeb " )
  }

  def triplesWithSubject(subj: IRI): Set[RDFTriple] = {
    val derefModel = ModelFactory.createDefaultModel
    RDFDataMgr.read(derefModel,subj.str)
    val model = QueryExecutionFactory.create(queryTriplesWithSubject(subj),derefModel).execConstruct()
    val triples = model2triples(model)
    log.info("triples with subject " + subj + " =\n" + triples)
    triples
  }
  
  def triplesWithObject(obj: IRI): Set[RDFTriple] = {
    val derefModel = ModelFactory.createDefaultModel
    RDFDataMgr.read(derefModel,obj.str)
    val model = QueryExecutionFactory.create(queryTriplesWithObject(obj),derefModel).execConstruct()
    model2triples(model)
  }

  def model2triples(model: Model): Set[RDFTriple] = {
    model.listStatements().map(st => statement2triple(st)).toSet
  }
  
  def statement2triple(st: Statement): RDFTriple = {
    RDFTriple(
        jena2rdfnode(st.getSubject), 
        property2iri(st.getPredicate),
        jena2rdfnode(st.getObject))
  }
  

  def property2iri(p: Property): IRI = {
    IRI(p.getURI)
  }
  
  def jena2rdfnode(r: JenaRDFNode): RDFNode = {
    if (r.isAnon) {
      BNodeId(r.asNode.getBlankNodeId().hashCode())
    } else 
   if (r.isURIResource) {
      IRI(r.asResource.getURI())
   } else 
   if (r.isLiteral) {
     val lit = r.asLiteral
     if (lit.getDatatypeURI() == null) {
       StringLiteral(lit.getString())
     } else
     lit.getDatatypeURI() match {
       case RDFNode.IntegerDatatypeIRI => IntegerLiteral(lit.getInt)  
       case RDFNode.BooleanDatatypeIRI => BooleanLiteral(lit.getBoolean)
       case RDFNode.DoubleDatatypeIRI => DoubleLiteral(lit.getDouble())
       case RDFNode.LangStringDatatypeIRI => LangLiteral(lit.getLexicalForm, Lang(lit.getLanguage))
       case _ => DatatypeLiteral(lit.getLexicalForm, IRI(lit.getDatatypeURI))       
     }    
   } else 
     throw new Exception("Unknown type of resource")
  }
  

}