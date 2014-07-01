package es.weso.shex

import scala.util._
import org.slf4j._
import es.weso.utils.Logging
import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import com.typesafe.config._
import es.weso.monads.Result._
import es.weso.parser.PrefixMap
import es.weso.rdf.RDF
import es.weso.rdf.RDFTriples
import es.weso.rdf.reader.Endpoint
import es.weso.rdf.reader.RDFFromWeb
import es.weso.rdfgraph.nodes.IRI
import es.weso.shex.ShapeDoc._
import es.weso.shex.ShapeSyntax._
import es.weso.shex.Typing._
import es.weso.utils.IO._
import com.hp.hpl.jena.sparql.procedure.library.debug


class Opts(
    arguments: Array[String],
    onError: (Throwable, Scallop) => Nothing
    ) extends ScallopConf(arguments) {

    banner("""| RDF validator using Shape Expressions
              | Options:
              |""".stripMargin)
    
    footer("Enjoy!")
    
    version("ShExcala 0.0.9")
    
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
                    required=false,
                    descr = "IRI")
                    
    val syntax 	= opt[String]("syntax", 
        			default=Some("ShEx"), 
        			descr = "ShEx")
        			
    val showSchema = toggle("showSchema", 
    				prefix = "no-",
    				default = Some(false),
    				descrYes = "show schema", 
        			descrNo = "don't show schema")
        			
    val showRDF   = toggle("showRDF", 
    				prefix = "no-",
    				default = Some(false),
    				descrYes = "show RDF", 
        			descrNo = "don't show RDF")

    val withIncoming   = toggle("withIncoming", 
    				prefix = "no-",
    				default = Some(false),
    				descrYes = "validates with nodes incoming", 
        			descrNo = "does not validate nodes incoming")        			
        			
    val withAny   = toggle("withAny", 
    				prefix = "no-",
    				default = Some(false),
    				descrYes = "adds a node of type any", 
        			descrNo = "does not add a node of type any")        			

    val withOpen   = toggle("withOpen", 
    				prefix = "no-",
    				default = Some(false),
    				descrYes = "with open semantics", 
        			descrNo = "with closed semantics")        			

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

object Main extends App with Logging {

 // val logger = Logger(LoggerFactory getLogger "name")

 override def main(args: Array[String]) {

  val conf 		= ConfigFactory.load()
  val opts 		= new Opts(args,errorDriver)
  
  if (opts.verbose()) {
    setDebug()
    log.debug("...in debug mode")
  }

  val rdf = opts.turtle.get match {
    case None => opts.endpoint.get match {
      case None => RDFFromWeb()
      case Some(endpoint) => Endpoint(endpoint)
    }
    case Some(turtleFile) => {
      log.debug("Reading from file " + turtleFile)
      val ts = getRDFFromTurtle(turtleFile).get
      log.debug("# of triples: " + ts.rdfTriples.size)
      ts
    } 
  }
  
  val result = 
    for ( (schema,pm) <- getSchema(opts.schema())
        ) yield {
   
   log.debug("Got schema. Labels: " + showLabels(schema))
   if (opts.showSchema()) {
      println(schema.toString())
    }
   
   if (opts.showRDF()) {
     println(rdf.toString())
   }
   log.debug("Before match schema ")
   if (opts.iri.isSupplied)
    (Schema.matchSchema(IRI(opts.iri()),rdf,schema,opts.withIncoming(),opts.withOpen(), opts.withAny()),pm)
   else 
    (Schema.matchAll(rdf,schema,opts.withIncoming(),opts.withOpen(),opts.withAny()),pm)
  }

  result match {
    case Success((ts,pm)) => {
      if (ts.isFailure) {
        println("<No shape typings>")
      } else
      for ((typing,n) <- (ts.run) zip (1 to 100) ) {
        println(s"Solution ${n}:\n" + typing.showTyping(pm))
      }
    }
    case Failure(f) => {
      println("Failure: " + f)
    }
  }
 }

 private def showLabels(schema: Schema): String = {
   schema.getLabels.map(_.getNode.toString ++ " ").mkString 
 }
 
 private def getSchema(fileName: String) : Try[(Schema,PrefixMap)] = {
	for (
        cs <- getContents(fileName) 
      ; (schema,prefixMap) <- Schema.fromString(cs)
      ) yield (schema,prefixMap)
 }

 private def getRDFFromTurtle(fileName: String) : Try[RDF] = {
  for ( cs <- getContents(fileName) 
      ; triples <- RDFTriples.parse(cs)
      ) yield {
    triples
  }
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

