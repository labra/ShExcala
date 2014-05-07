package es.weso.shex


import com.hp.hpl.jena.rdf.model.RDFNode
import es.weso.rdfgraph.nodes._
import com.typesafe.config._
import com.hp.hpl.jena.rdf.model.ModelFactory
import com.hp.hpl.jena.rdf.model.Model
import com.hp.hpl.jena.rdf.model.Resource
import java.io.ByteArrayInputStream
import java.io.InputStream
import scala.io.Source._
import es.weso.jena.JenaMapper
import com.hp.hpl.jena.rdf.model.Property
import es.weso.parser.TurtleParser
import java.io.IOException
import java.io.FileNotFoundException
import scala.util.Try._
import scala.util.Failure
import java.net.URI
import scala.util.Success
import es.weso.rdf.RDFTriples
import scala.util.Try

object RunTestsFolder {

  val conf : Config = ConfigFactory.load()
  
  val manifestFile 	= conf.getString("manifestFile")
  val testsDir 		= conf.getString("TurtleTestsDir")
  val rdf  			= "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  val shext 		= "http://www.w3.org/ns/shextest#"
  val mf 			= "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#"
  val shExTestsURL 	= "http://www.w3.org/2013/ShExTests/"

  val model 		= ModelFactory.createDefaultModel
  val syntax	 	= model.createProperty(shext + "SyntaxOk")
  val valid		 	= model.createProperty(shext + "Valid")
  val nonValid		= model.createProperty(shext + "NonValid")
  val schema		= model.createProperty(shext + "schema")
  val instance		= model.createProperty(shext + "instance")
  val action 		= model.createProperty(mf 	+ "action")
  val result 		= model.createProperty(mf 	+ "result")
  val mfname 		= model.createProperty(mf 	+ "name")
  val rdftype 		= model.createProperty(rdf 	+ "type")

  def createReport : Report = {
	
    model.read(manifestFile, testsDir,"TURTLE")

    val positiveSyntaxRs = get_resources(model,shext+"PositiveSyntax")
    val negativeSyntaxRs = get_resources(model,shext+"NegativeSyntax")
    val validRs 		 = get_resources(model,shext+"Valid")
    val nonValidRs 		 = get_resources(model,shext+"NonValid")
      
    val positiveSyntax 	= reportPositiveSyntax(model,positiveSyntaxRs)
    val negativeSyntax 	= reportNegativeSyntax(model,negativeSyntaxRs)
    val valid	   		= reportValid(model,validRs)

    //    val nonValid   = reportNonValid(model,nonValidRs)

    println("Positive syntax #:" + positiveSyntaxRs.size)
    println("Negative syntax #:" + negativeSyntaxRs.size)
    println("Valid #:" + validRs.size)
    positiveSyntax concat negativeSyntax concat valid // concat nonValid
  }  

   def get_resources(m : Model, t:String): List[Resource] = {
     var resultSet = scala.collection.mutable.Set[Resource]()
     val resType = m.createProperty(t)
     val iter = m.listSubjectsWithProperty(rdftype,resType)
       while (iter.hasNext) {
         resultSet += iter.next
       }
     resultSet.toList
   }
   
   def reportPositiveSyntax(m:Model,rs:List[Resource]) : Report = 
     rs.foldLeft(Report.initial)(positiveSyntax(m))

   def reportNegativeSyntax(m:Model,rs:List[Resource]) : Report = 
     rs.foldLeft(Report.initial)(negativeSyntax(m))

   def positiveSyntax(m:Model)(currentReport : Report,r: Resource): Report =  {     
       val schema = getSchema(m,r)
       val name   = getName(m,r)
       val baseIRI = shExTestsURL + name + ".ttl" // Base URI for relative URI resolution. Similar to http://www.w3.org/2013/TurtleTests/
       val testType="Positive syntax"
       schema match {
         case Some(node) if node.isURIResource => {
        	 val contents = fromURL(node.asResource().getURI).mkString
        	 val result = for ( 
        	     (schema,pm) <- Schema.fromString(contents) 
        	 ) yield (schema)
        	 result match {
        	   case Success(_) => 
        		 	  currentReport.addTestReport(true, name, 
        		 	      r.getLocalName, testType, " Parsed OK")
        	   case Failure(msg) => {
        		 	  currentReport.addTestReport(false, name, r.getLocalName, testType, " Parsing failed:\n" + msg)
        		 	}
        	 }
          }
          case _ => 
            currentReport.addTestReport(false, name, r.getLocalName, testType, "Cannot retrieve action for resource " + r)
       }
   }

   def negativeSyntax(m:Model)(currentReport : Report,r: Resource): Report =  {     
       val schema = getSchema(m,r)
       val name   = getName(m,r)
       val baseIRI = shExTestsURL + name + ".ttl" // Base URI for relative URI resolution. See http://www.w3.org/2013/TurtleTests/
       val testType="Negative syntax"
       schema match {
         case Some(node) if node.isURIResource => {
        	 val contents = fromURL(node.asResource().getURI()).mkString 
        	 try { 
        	   ShapeParser.parse(contents,IRI(baseIRI)) match {
        		 	case util.Success(_) => 
        		 	  currentReport.addTestReport(false, name, r.getLocalName, testType, "Parsed OK when expected to fail parsing")
        		 	case util.Failure(msg) => 
        		 	  currentReport.addTestReport(true,name, r.getLocalName, testType, "Parsing failed with message: " + msg)
        		 }
        	 } catch {
        	  case e:Throwable => 
       		      currentReport.addTestReport(false,name, r.getLocalName, testType,"Exception raised when parsing: " + e)
        	 }
          }
          case _ => 
            currentReport.addTestReport(false, name, r.getLocalName, testType, "Cannot retrieve action for resource " + r)
       }
   }
  
   def reportValid(m:Model,rs:List[Resource]) : Report =
     rs.foldLeft(Report.initial)(valid(m))
     
   def valid(m:Model)(currentReport : Report,res: Resource): Report = {
       val schema 	= getSchema(m,res)
       val instance = getInstance(m,res)
       val name   = getName(m,res)
       val testType = "Valid"
       val baseIRI = shExTestsURL + name + ".ttl" // Base URI for relative URI resolution. See http://www.w3.org/2013/TurtleTests/
       (schema,instance) match {
         case (Some(s),Some(i)) 
         if s.isURIResource && i.isURIResource => {
        	 val uriSchema 	 = s.asResource().getURI
        	 val uriInstance = i.asResource().getURI

        	 valid(name,IRI(baseIRI),uriSchema,uriInstance) match {
        	   case Success(_) => currentReport.addTestReport(true,name,res.getLocalName, testType,"Valid")
        	   case Failure(e) => currentReport.addTestReport(false, name, res.getLocalName, testType, e.getMessage())
        	 }
         }
         case x => 
           currentReport.addTestReport(false, name, res.getLocalName, testType, "Cannot retrieve (action,result) for resource " + res + ". Obtained: " + x)
       }
   }

/*   def reportNonValid(m:Model,rs:List[Resource]) : Report =
     rs.foldLeft(Report.initial)(nonValid(m))
     
   def nonValid(m:Model)(currentReport : Report,r: Resource): Report = {
       val action = getAction(m,r)
       val name   = getName(m,r)
       val testType = "Negative Turtle Eval"
       val baseIRI = shExTestsURL + name + ".ttl" // Base URI for relative URI resolution. See http://www.w3.org/2013/TurtleTests/
       action match {
         case Some(a) 
         if a.isURIResource => {
        	 val strAction = fromURL(a.asResource().getURI,"UTF-8").mkString

         	 try { 
        	   TurtleParser.parse(strAction,IRI(baseIRI)) match {
        		 	case util.Success((triples,pm)) => 
        		 	  val model = RDFTriples2Model(triples)
        		 	  currentReport.addTestReport(false, name, r.getLocalName, testType, "Parsed and model built when expected to fail parsing")
        		 	case util.Failure(msg) => 
        		 	  currentReport.addTestReport(true,name,r.getLocalName, testType, "Parsing failed with message: " + msg)
        		 }
        	 } catch {
        	  case e:Throwable => 
       		      currentReport.addTestReport(true,name,r.getLocalName, testType,"Exception raised when parsing: " + e)
        	 }
         }
         case x => 
           currentReport.addTestReport(false, name, r.getLocalName, testType, "Cannot retrieve action for resource " + r)
       }
   }
  */ 
   def getSchema(m: Model, r:Resource) : Option[RDFNode] = {
     getPropertyOfResource(m,r,schema)
   }

   def getInstance(m: Model, r:Resource) : Option[RDFNode] = {
     getPropertyOfResource(m,r,instance)
   }

   def getPropertyOfResource(m: Model, r:Resource, p:Property) : Option[RDFNode] = {
     val iter = m.listObjectsOfProperty(r,p)
     // TODO: Check that there are no more than one
     if (iter.hasNext) {
       val node : RDFNode = iter.next()
       Some(node)
     } else None
   }

   def getName(m: Model, r:Resource) : String = {
     val iter = m.listObjectsOfProperty(r,mfname)
     if (iter.hasNext) {
       val node : RDFNode = iter.next()
       if (node.isLiteral) node.asLiteral().getLexicalForm
       else "<resource " + r + " with no name>"
     } else "<resource " + r + " with no name>"
   }

  def valid( name:String,
		   baseIRI: IRI,
		   uriSchema : String, 
		   uriInstance: String ) : Try[String]= {
   for ( cs_schema <- dereference(uriSchema)  
       ; (schema,prefixMap) <- Schema.fromString(cs_schema)
       ; cs_instance <- dereference(uriInstance)
       ; rdf <- RDFTriples.parse(cs_instance)
       ) yield {
    println("Validating. RDF: " + rdf.toString + "Schema: " + schema.toString)
    val ctx = Context(rdf = rdf, shEx = schema.shEx)
    val result = ShapeValidator.matchAll(ctx)
    println("\nResult:" + result.toString)
    if (result.isValid) "Valid result"
    else throw new Exception("Non valid")
   }
  }

 /**
  * Convert a String to a Model
  * @param s String
  * @param base Base URL (default = empty String)
  * @param lang Syntax language. Can be: RDF/XML, N-TRIPLES, TURTLE. Default value: TURTLE
  */
 def str2model(s: String, 
		 base:String = "", 
		 lang:String = "TURTLE") : Model = {
   val m = ModelFactory.createDefaultModel
   val in : InputStream = new ByteArrayInputStream(s.getBytes("UTF-8"))
   m.read(in,base,lang)
   m
 }

 def getResourceWithName(name: String, m: Model) : Option[Resource] = {
     val iter = m.listSubjectsWithProperty(mfname,name)
     if (iter.hasNext) {
       val node : Resource = iter.next()
       Some(node)
     } else None
 }
 
 def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B = {
   try {
	 f(resource)
   } finally {
	 resource.close()
   }
 }

 def dereference(uri: String): Try[CharSequence] = {
  try {
    using(io.Source.fromURL(uri)){ source => 
      Success(source.getLines.mkString("\n"))
     }
  } catch {
  	case e: FileNotFoundException => { 
  	  Failure(e)
  	}
  	case e: IOException => {
  	  Failure(e)
  	}
  }
 }

}


 
