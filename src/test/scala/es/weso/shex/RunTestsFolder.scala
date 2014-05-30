package es.weso.shex


import com.hp.hpl.jena.rdf.model.{RDFNode => JenaNode}
import es.weso.rdfgraph.nodes._
import es.weso.monads.Result._
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
import scala.collection.JavaConverters._

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
  val iri			= model.createProperty(shext + "iri")  
  val shape			= model.createProperty(shext + "shape")  
  val action 		= model.createProperty(mf 	+ "action")
  val result 		= model.createProperty(mf 	+ "result")
  val mfname 		= model.createProperty(mf 	+ "name")
  val rdftype 		= model.createProperty(rdf 	+ "type")

  def createReport : Report = {
	
    model.read(manifestFile, testsDir,"TURTLE")

    val positiveSyntaxRs = get_resources(model,shext+"PositiveSyntax")
    val negativeSyntaxRs = get_resources(model,shext+"NegativeSyntax")
    val validRs 		 = get_resources(model,shext+"Valid")
    val notValidRs 		 = get_resources(model,shext+"NotValid")
      
    val positiveSyntax 	= reportPositiveSyntax(model,positiveSyntaxRs)
    val negativeSyntax 	= reportNegativeSyntax(model,negativeSyntaxRs)
    val valid	   		= reportValid(model,validRs)
    val notValid	   	= reportNotValid(model,notValidRs)

    println("Positive syntax #:" + positiveSyntaxRs.size)
    println("Negative syntax #:" + negativeSyntaxRs.size)
    println("Valid #:" + validRs.size)
    println("Not Valid #:" + notValidRs.size)
    positiveSyntax concat negativeSyntax concat valid concat notValid
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
   
   // Todo...refactor this...
   def reportPositiveSyntax(m:Model,rs:List[Resource]) : Report = 
     rs.foldLeft(Report.initial)(positiveSyntax(m))

   def reportNegativeSyntax(m:Model,rs:List[Resource]) : Report = 
     rs.foldLeft(Report.initial)(negativeSyntax(m))

   def reportValid(m:Model,rs:List[Resource]) : Report =
     rs.foldLeft(Report.initial)(valid(m))
     
   def reportNotValid(m:Model,rs:List[Resource]) : Report =
     rs.foldLeft(Report.initial)(notValid(m))

   def positiveSyntax(m:Model)(
       currentReport : Report, 
       r: Resource): Report =  {
     val testType="Positive syntax"
     val vreport = for 
         ( schema <- getSchema(m,r)
         ; name   <- getName(m,r)
         ; cs_schema <- dereference(schema.str)
         ) yield (cs_schema,name)

     vreport match {
         case Success((cs_schema,name)) =>
         	Schema.fromString(cs_schema) match {
         		case Success((schema,_)) => 
        		  currentReport.addTestReport(true, name, 
        	 	      r.getLocalName, testType, " Parsed OK")
         		case Failure(msg) => {
         		  currentReport.addTestReport(false, name, r.getLocalName, testType, " Parsing failed:\n" + msg)
                }
            }
         case Failure(msg) => {
           currentReport.addTestReport(false, r.getLocalName, r.getLocalName, testType, " Bad test:\n" + msg)
         }
       }
   }

   def negativeSyntax(m:Model)(currentReport : Report,r: Resource): Report =  {     
     val testType="Negative syntax"
     val vreport = for 
         ( schema <- getSchema(m,r)
         ; name   <- getName(m,r)
         ; cs_schema <- dereference(schema.str)
         ) yield (cs_schema,name)

     vreport match {
         case Success((cs_schema,name)) =>
         	Schema.fromString(cs_schema) match {
         		case Success((schema,_)) => 
        		  currentReport.addTestReport(false, name, r.getLocalName, testType, "Parsed OK when expected to fail parsing")
         		case Failure(msg) => {
         		  currentReport.addTestReport(true,name, r.getLocalName, testType, "Parsing failed with message: " + msg)
                }
            }
         case Failure(msg) => {
           currentReport.addTestReport(false, r.getLocalName, r.getLocalName, testType, " Bad test:\n" + msg)
         }
       }
   }
  

   def valid(m:Model)(
       currentReport : Report,
       res: Resource): Report = {
     val testType = "Valid"
     val vreport = 
       for ( schema   <- getSchema(m,res)
           ; instance <- getInstance(m,res)
           ; iri 	  <- getIRI(m,res)
           ; shapes	  <- getShapes(m,res)
           ; name     <- getName(m,res) 
           ) yield {
       val baseIRI = shExTestsURL + name + ".ttl" // Base URI for relative URI resolution. See http://www.w3.org/2013/TurtleTests/
       (valid(name,IRI(baseIRI),schema,instance,iri,shapes),name) 
       }
    vreport match {
    	case Success((r,name)) => {
    	  r match {
    	    case Success(msg) => currentReport.addTestReport(true,name, res.getLocalName, testType, msg)
    	    case Failure(e) => currentReport.addTestReport(false, name, res.getLocalName, testType, e.getMessage())
    	  }
    	}
       	case Failure(e) => {
       	  currentReport.addTestReport(false, res.getURI, res.getLocalName, testType, e.getMessage())
       	}
    }
   }

   def notValid(m:Model)(currentReport : Report,res: Resource): Report = {
     val testType = "NotValid"
     val vreport = 
       for ( schema   <- getSchema(m,res)
           ; instance <- getInstance(m,res)
           ; iri 	  <- getIRI(m,res)
           ; name     <- getName(m,res) 
           ) yield {
       val baseIRI = shExTestsURL + name + ".ttl" // Base URI for relative URI resolution. See http://www.w3.org/2013/TurtleTests/
       (notValid(name,IRI(baseIRI),schema,instance,iri),name) 
       }
    vreport match {
    	case Success((r,name)) => {
    	  r match {
    	    case Success(msg) => currentReport.addTestReport(true,name, res.getLocalName, testType, msg)
    	    case Failure(e) => currentReport.addTestReport(false, name, res.getLocalName, testType, e.getMessage())
    	  }
    	}
       	case Failure(e) => {
       	  currentReport.addTestReport(false, res.getURI, res.getLocalName, testType, e.getMessage())
       	}
    }
   }

   def getSchema(m: Model, r:Resource) : Try[IRI] = {
     getURIWithProperty(m,r,schema)
   }

   def getInstance(m: Model, r:Resource) : Try[IRI] = {
     getURIWithProperty(m,r,instance)
   }

   def getIRI(m: Model, r:Resource) : Try[IRI] = {
     getURIWithProperty(m,r,iri)
   }

   def getShapes(m: Model, r:Resource) : Try[Set[RDFNode]] = {
     for ( rs <- getResourcesWithProperty(m,r,shape)
         ) yield (rs.map(r => IRI(r.asResource.getURI)).toSet)
   }

   def getURIWithProperty(m: Model, r:Resource, p:Property) : Try[IRI] = {
     for (rs <- getResourcesWithProperty(m,r,p)) 
     yield {
       if (rs.size == 1) {
         val r = rs.head 
         if (r.isURIResource()) IRI(r.asResource.getURI)
         else throw new Exception("getURIWithProperty: value of property " + p + " is " + r + ", must be an URI")
       } 
       else throw new Exception("getURIWithProperty: resource " + r + " has more than one value for property " + p)
     }
   }

   def getResourcesWithProperty(m: Model, r:Resource, p:Property) : Try[List[JenaNode]] = {
     if (m.contains(r,p)) 
       Success(m.listObjectsOfProperty(r,p).toList.asScala.toList)
     else 
       Failure(new Exception("Resource "+ r + "does not have property " + p))    
   }

   def getName(m: Model, r:Resource) : Try[String] = {
     val iter = m.listObjectsOfProperty(r,mfname)
     if (iter.hasNext) {
       val node : JenaNode = iter.next()
       if (node.isLiteral) Success(node.asLiteral().getLexicalForm)
       else Failure(new Exception("getName: resource " + r + ", name = " + node + " is not a literal"))
     } 
     else Failure( new Exception("getName: resource " + r + " with no name"))
   }

  def valid(name:String,
		    baseIRI: IRI,
		    schema : IRI, 
		    instance: IRI,
		    iri: IRI, 
		    shapes: Set[RDFNode]) : Try[String]= {
   for ( cs_schema <- dereference(schema.str)  
       ; (schema,prefixMap) <- Schema.fromString(cs_schema)
       ; cs_instance <- dereference(instance.str)
       ; rdf <- RDFTriples.parse(cs_instance)
       ) yield {
    val result = Schema.matchSchema(iri, rdf, schema)
    if (result.isValid) {
      val typings = result.toList
      if (typings.exists(t => t.hasTypes(iri,shapes))) {
       "Valid typings: " + typings
      } else {
        throw new Exception("Result does not contain " + iri + " -> " + shapes + "\nTypings: " + typings)
      }
    }
    else 
      throw new Exception("Non valid")
   }
  }

  def notValid(name:String,
		    baseIRI: IRI,
		    schema : IRI, 
		    instance: IRI,
		    iri: IRI 
		    ) : Try[String]= {
   for ( cs_schema <- dereference(schema.str)  
       ; (schema,prefixMap) <- Schema.fromString(cs_schema)
       ; cs_instance <- dereference(instance.str)
       ; rdf <- RDFTriples.parse(cs_instance)
       ) yield {
    val result = Schema.matchSchema(iri, rdf, schema)
    if (result.isValid) {
        throw new Exception("Result valid with typings: " + result.toList + " but should not be valid")
    }
    else 
      "Not valid"
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


 
