package es.weso.shex

import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import org.slf4j._
import com.typesafe.config._
import es.weso.shex.ShapeSyntax._
import es.weso.shex.ShapeDoc._
import scala.util.parsing.input.CharSequenceReader
import java.io.IOException
import java.io.FileNotFoundException
import scala.util._
import es.weso.utils.IO._
import es.weso.rdf.RDFTriples
import es.weso.rdf.RDF

class Opts(
    arguments: Array[String],
    onError: (Throwable, Scallop) => Nothing
    ) extends ScallopConf(arguments) {

    banner("""| ShEx validator
              | Options:
              |""".stripMargin)
    footer("Enjoy!")
    version("ShExcala 0.1")
    val rdf     = opt[String]("rdf",
    				required=false,
    				descr = "RDF Data file")
    val schema 	= opt[String]("schema",
    				required=true,
    				descr = "Schema file")
    val syntax 	= opt[String]("syntax", 
        			default=Some("ShEx"), 
        			descr = "ShEx")
    val showSchema = toggle("show", 
    				prefix = "no-",
    				default = Some(false),
    				descrYes = "show schema", 
        			descrNo = "don't show schema")
    val showRDF   = toggle("showData", 
    				prefix = "no-",
    				default = Some(false),
    				descrYes = "show data", 
        			descrNo = "don't show data")
    val verbose    = toggle("verbose", 
    				prefix = "no-",
    				default = Some(false),
    				descrYes = "Normal output", 
        			descrNo = "Verbose output")
    val output  = opt[String]("out",
    				descr = "Output validation typing to file")
    val version = opt[Boolean]("version", 
    				noshort = true, 
    				descr = "Print version")
    val help 	= opt[Boolean]("help", 
    				noshort = true, 
    				descr = "Show this message")
  
  override protected def onError(e: Throwable) = onError(e, builder)
}

object Main extends App {


 override def main(args: Array[String]) {

  val log 		= LoggerFactory.getLogger("Application")
  val conf 		= ConfigFactory.load()
  val opts 		= new Opts(args,errorDriver)

  val result = 
    for ( schema <- getSchema(opts.schema())
        ; rdf <- getRDF(opts.rdf())
        ) yield {
   if (opts.showSchema()) {
      println(schema.toString())
    }
   if (opts.showRDF()) {
     println(rdf.toString())
   }
   val ctx = Context(rdf = rdf, shEx = schema.shEx)
   ShapeValidator.matchAll(ctx)
  }

  result match {
    case Success(ts) => println("Success: " + ts.toString)
    case Failure(f) => println("Failure: " + f.toString)
  }
 }
 
 private def getSchema(fileName: String) : Try[Schema] = {
  for (
        cs <- getContents(fileName) 
      ; (schema,prefixMap) <- Schema.fromString(cs)
      ) yield schema
 }

 private def getRDF(fileName: String) : Try[RDF] = {
  for (
        cs <- getContents(fileName) 
      ; triples <- RDFTriples.parse(cs)
      ) yield triples
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

