package es.weso.shex

import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import org.slf4j._
import com.typesafe.config._
import es.weso.shex.ShapeSyntax._
import es.weso.shex.ShapeDoc._
import es.weso.monads.Result._
import es.weso.shex.Typing._
import scala.util.parsing.input.CharSequenceReader
import java.io.IOException
import java.io.FileNotFoundException
import scala.util._
import es.weso.utils.IO._
import es.weso.rdf.RDFTriples
import es.weso.rdf.RDF
import es.weso.rdfgraph.nodes.IRI
import es.weso.parser.PrefixMap
import es.weso.rdf.reader.Endpoint
import es.weso.rdf.reader.RDFFromWeb
import org.apache.log4j.LogManager
import org.apache.log4j.Level


class Opts(
    arguments: Array[String],
    onError: (Throwable, Scallop) => Nothing
    ) extends ScallopConf(arguments) {

    banner("""| RDF validator using Shape Expressions
              | Options:
              |""".stripMargin)
    
    footer("Enjoy!")
    
    version("ShExcala 0.0.2")
    
    val turtle     = opt[String]("turtle",
    				   required=false,
    				   descr = "Turtle RDF Data file")
    				
    val endpoint   = opt[String]("endpoint",
                      required=false,
                      descr = "SPARQL endpoint")
                    
    val schema 	= opt[String]("schema",
    				required=true,
    				descr = "Schema file")
    				
    val iri 	= opt[String]("iri",
                    required=true,
                    descr = "IRI")
                    
    val syntax 	= opt[String]("syntax", 
        			default=Some("ShEx"), 
        			descr = "ShEx")
        			
    val showSchema = toggle("show", 
    				prefix = "no-",
    				default = Some(false),
    				descrYes = "show schema", 
        			descrNo = "don't show schema")
        			
    val showRDF   = toggle("showRDF", 
    				prefix = "no-",
    				default = Some(false),
    				descrYes = "show RDF", 
        			descrNo = "don't show RDF")
        			
    val verbose = toggle("verbose", 
    				prefix = "no-",
    				default = Some(false),
    				descrYes = "Normal output", 
        			descrNo = "Verbose output")
        			
    val version = opt[Boolean]("version", 
    				noshort = true, 
    				descr = "Print version")
    				
    val help 	= opt[Boolean]("help", 
    				noshort = true, 
    				descr = "Show this message")
  
    mutuallyExclusive(turtle,endpoint)
    
    override protected def onError(e: Throwable) = onError(e, builder)
}

object Main extends App {


 override def main(args: Array[String]) {

  val conf 		= ConfigFactory.load()
  val opts 		= new Opts(args,errorDriver)
  if (opts.verbose()) {
    LogManager.getRootLogger().setLevel(Level.DEBUG);
  }
  val log 		= LoggerFactory.getLogger("Main")

  val rdf = opts.turtle.get match {
    case None => opts.endpoint.get match {
      case None => RDFFromWeb()
      case Some(endpoint) => Endpoint(endpoint)
    }
    case Some(turtleFile) => {
      getRDFFromTurtle(turtleFile).get
    } 
  }
  
  val result = 
    for ( (schema,pm) <- getSchema(opts.schema())
        ; iri <- getIRI(opts.iri())
        ) yield {
   
   log.debug("Got schema " + schema)
   log.debug("Got iri " + iri)
   if (opts.showSchema()) {
      println(schema.toString())
    }
   
   if (opts.showRDF()) {
     println(rdf.toString())
   }
   log.debug("Before match schema ")
   (Schema.matchSchema(iri,rdf,schema),pm)
  }

  log.debug("Before result check, result = " + result)

  result match {
    case Success((ts,pm)) => {
      if (ts.isFailure) {
        println("<No shape typings>")
      } else
      for ((typing,n) <- (ts.run) zip (1 to 100) ) {
        println(s"Solution ${n}:\n" + typing.showTyping(pm))
      }
    }
    case Failure(f) => println("Failure: " + f.toString)
  }
 }
 
 private def getSchema(fileName: String) : Try[(Schema,PrefixMap)] = {
  for (
        cs <- getContents(fileName) 
      ; (schema,prefixMap) <- Schema.fromString(cs)
      ) yield (schema,prefixMap)
 }

 private def getRDFFromTurtle(fileName: String) : Try[RDF] = {
  for (
        cs <- getContents(fileName) 
      ; triples <- RDFTriples.parse(cs)
      ) yield triples
 }

 private def getIRI(str: String) : Try[IRI] = {
   Success(IRI(str))
 }

 private def errorDriver(e: Throwable, scallop: Scallop) = e match {
    case Help(s) =>
      println("Help: " + s)
      scallop.printHelp
      sys.exit(0)
    case _ =>
      println("Error: %s".format(e.getMessage))
      scallop.printHelp
      sys.exit(1)
  }
  
} 

