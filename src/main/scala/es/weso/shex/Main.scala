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

class Opts(
    arguments: Array[String],
    onError: (Throwable, Scallop) => Nothing
    ) extends ScallopConf(arguments) {

    banner("""| ShEx validator
              | Options:
              |""".stripMargin)
    footer("Enjoy!")
    version("ShExcala 0.1")
    val data 	= opt[String]("data",
    				required=false,
    				descr = "Data file")
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

  val schemaFile = opts.schema()
  for (cs <- getContents(schemaFile); 
      (schema,prefixMap) <- Schema.fromString(cs) 
  ) {
    if (opts.showSchema()) {
      println(schema.toString())
    }
/*    val dataFile = opts.data()
    if (dataFile != null) {
      for (cs <- getContents(dataFile); 
          (data,prefixMapData) <- RDF.fromString(cs)) {
        if (opts.showData()) {
          println(data.toString())
        }
        
      }  
    } */
  }
/*  getContents(schemaFile) match {
    case Failure(e) => 
      println("Exception parsing file " + schemaFile + ": " + e.getLocalizedMessage())
    case Success(cs) => {
      log.info("Input string:\n" + cs)
      Schema.fromString(cs) match {
        case Success((schema,prefixMap)) => 
           if (opts.showSchema()) {
        	 log.info("Schema parsed")
        	 println(schema.toString())
    	   }
        case Failure(e) => println("Exception parsing:" + e)
      }
    }
   } */
 }

 /**
  * Ensures to close a file
  * [[https://wiki.scala-lang.org/display/SYGN/Loan Loan pattern]]
  */
 def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B = {
   try {
	 f(resource)
   } finally {
	 resource.close()
   }
 }
  

 def getContents(fileName: String): Try[CharSequence] = {
  try {
    using(io.Source.fromFile(fileName)){ source => 
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

