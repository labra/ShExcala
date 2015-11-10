package es.weso.rdf.validator

import scala.util._
import org.slf4j._
import es.weso.utils.Logging
import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import com.typesafe.config._
import es.weso.rdf._
import es.weso.rdf.jena._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.utils.IO._
import buildinfo._
import es.weso.shacl._
import es.weso.rdfgraph.nodes._
import es.weso.utils.Verbosity
import es.weso.shacl.Shacl._

case class MainException(msg: String) extends Exception(s"Exception in main: $msg")

case object ValidatorVersions {
  lazy val validatorVersions = List("SHEX3")

  def available(x: String): Boolean = {
    validatorVersions.contains(x)
  }

  def default = validatorVersions(0)

}

class Opts(
  arguments: Array[String],
  onError: (Throwable, Scallop) => Nothing) extends ScallopConf(arguments) {

  banner("""| RDF validator using Shape Expressions
            | Options:
            |""".stripMargin)

  footer("Enjoy!")

  version(BuildInfo.version)

  val data = opt[String]("data",
    short = 'd',
    required = false,
    descr = "Data to validate")

  val data_format = opt[String]("in-data-format",
    short = 'f',
    required = false,
    default = Some("TURTLE"),
    validate = (x => DataFormats.available(x)),
    descr = "Input data format. Available values: " + DataFormats.toString)

  val outdata_format = opt[String]("out-data-format",
    required = false,
    default = Some("TURTLE"),
    descr = "Output data format. Available values: " + DataFormats.toString,
    validate = (x => DataFormats.available(x)),
    noshort = true)

  val endpoint = opt[String]("endpoint",
    short = 'e',
    required = false,
    descr = "SPARQL endpoint")

  val node = opt[String]("node",
    short = 'n',
    required = false,
    descr = "Node to validate")

  val shape_label = opt[String]("shape",
    short = 'l',
    required = false,
    descr = "Label (IRI) of Shape in Schema")

  val schema = opt[String]("schema",
    short = 's',
    required = false,
    descr = "Schema file"
    )

  val schema_format = opt[String]("schema-format",
    short = 'x',
    default = Some(SchemaFormats.default),
    descr = "Schema Format. Available values: " + SchemaFormats.toString,
    validate = (x => SchemaFormats.available(x)))

  val schema_version = opt[String]("schema-version",
    short = 'r',
    default = Some(ValidatorVersions.default),
    descr = "Schema Version. Available values: " + ValidatorVersions.toString,
    validate = (x => ValidatorVersions.available(x)))

  val outschema_format = opt[String]("out-schema-format",
    short = 'u', 
    required = false,
    default = Some("SHEXC"),
    descr = "Output schema format. Available values: " + SchemaFormats.toString,
    validate = (x => SchemaFormats.available(x))
    )

  val outschema_file = opt[String]("out-schema",
    short = 'o', 
    required = false,
    descr = "Output schema file"
    )
    
  val showSchema = toggle("showSchema",
    prefix = "no-",
    default = Some(false),
    descrYes = "show schema",
    descrNo = "don't show schema",
    noshort = true
    )

  val time = toggle("time",
    prefix = "no-",
    default = Some(false),
    descrYes = "show time",
    descrNo = "don't time",
    short = 't'
    )

  val memory = toggle("memory",
    prefix = "no-",
    default = Some(false),
    descrYes = "show memory used",
    descrNo = "don't show memory used",
    short = 'm')

  val showData = toggle("showData",
    prefix = "no-",
    default = Some(false),
    descrYes = "show Data",
    descrNo = "don't show Data",
    noshort = true
    )

  val cut = opt[Int](
    short = 'c',
    default = Some(1),
    noshort=true,
    validate = (0<))

  val withIncoming = toggle("withIncoming",
    prefix = "no-",
    default = Some(false),
    descrYes = "validates with nodes incoming",
    descrNo = "does not validate nodes incoming",
    noshort=true
    )

  val withAny = toggle("withAny",
    prefix = "no-",
    default = Some(false),
    descrYes = "adds a node of type any",
    descrNo = "does not add a node of type any",
    noshort=true
    )

  val withOpen = toggle("withOpen",
    prefix = "no-",
    default = Some(true),
    descrYes = "with open shapes by default",
    descrNo = "with closed shapes by default",
    noshort = true
    )

  val validator = opt[String](
    default = Some("SHEX3"),
    descr = "Validation algorithm: SHEX3 (Shape Expressions)",
    noshort=true,
    validate = (x => List("SHEX3").contains(x.toUpperCase))
)

  val verbose = toggle("verbose",
    prefix = "no-",
    default = Some(false),
    descrYes = "Normal output",
    descrNo = "Verbose output",
    short = 'V'
  )

  val version = toggle("version",
    prefix = "no-",
    default = Some(false),
    descrYes = "Show version",
    descrNo = "Don't show version",
    short = 'v'
    )

  val help = toggle("help",
    default = Some(false),
    descrYes = "Show help message",
    descrNo = "Don't show help message",
    short = 'h'
    )

  mutuallyExclusive(data, endpoint)

  override protected def onError(e: Throwable) = onError(e, builder)
}

object Main extends App with Verbosity {

  // val logger = Logger(LoggerFactory getLogger "name")

  override def main(args: Array[String]): Unit = {
    
    val conf = ConfigFactory.load()
    val opts = new Opts(args, errorDriver)
    
    if (args.length == 0) {
      opts.printHelp()
      return
    } 
    

    if (opts.version()) { 
      println("Show version")
      showVersion()
    }
    
    val isVerbose = opts.verbose()
    setVerbosity(isVerbose)
    
    val cut = opts.cut()

    val maybe_rdf: Option[RDFReader] = opts.data.get match {
      case None => opts.endpoint.get match {
        case None           => None // RDFFromWeb()
        case Some(endpoint) => Some(Endpoint(endpoint))
      }
      case Some(dataFile) => {
        log.debug("Reading from file \'" + dataFile + "\'...")
        val ts = getData(dataFile, opts.data_format()).get
        Some(ts)
      }
    }

    if (opts.showData() && maybe_rdf.isDefined) {
      println(maybe_rdf.get.serialize(opts.outdata_format()))
    }

    val now = getTimeNow()
    verbose("Instant = " + now)

    if (opts.schema.isDefined) {
      verbose("Schema defined ")

      val micros = getTimeFrom(now)
      val runtime = Runtime.getRuntime()
      if (maybe_rdf.isDefined) {
        val rdf = maybe_rdf.get
        validateSchema(rdf, opts)
      } else {
        convertSchema(opts)
      }

      if (opts.time()) { showTime(micros) }
      if (opts.memory()) { showRuntimeMemory(runtime) }

    } else { // If no schema...check to validate nodeShape declarations in RDF
      if (maybe_rdf.isDefined) {
        val attempts = validateRDF(maybe_rdf.get)
        showAttempts(attempts,isVerbose,cut,PrefixMaps.commonShacl)
      }
      
    }
  }

  private def showVersion(): Unit = {
    println("** Version: " + BuildInfo.version)
    println("** Scala version: " + BuildInfo.scalaVersion)
    println("** SBT version: " + BuildInfo.sbtVersion)
  }
  
  def getTimeNow(): Long = System.nanoTime
  
  def getTimeFrom(from: Long): Long = (System.nanoTime - from) / 1000
  
  def showTime(micros: Long): Unit = {
    println("** %d microseconds".format(micros))
  }
  
  def showAttempts(attempts: Try[Seq[ValidationAttempt[RDFNode,Label]]], verbose: Boolean, cut:Int, pm: PrefixMap): Unit = {
    attempts match {
      case Failure(e) => println("Exception trying to validate RDF: " + e)
      case Success(as) => {
        if (as.isEmpty) {
          println("Validation: No declaration of node shapes found")
        } else {
          as.foreach { showAttempt(_,verbose,cut,pm) }
        }
      }
    }
  }

  def showAttempt(attempt: ValidationAttempt[RDFNode,Label], verbose: Boolean, cut: Int, pm: PrefixMap): Unit = {
    attempt.show(verbose,cut,pm)
  }
  
  def showRuntimeMemory(runtime: Runtime): Unit = {
    // memory info, code from: http://alvinalexander.com/scala/how-show-memory-ram-use-scala-application-used-free-total-max
    val mb = 1024 * 1024
    println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
    println("** Free Memory:  " + runtime.freeMemory / mb)
    println("** Total Memory: " + runtime.totalMemory / mb)
    println("** Max Memory:   " + runtime.maxMemory / mb)
  }

  private def getData(data: String, format: String): Try[RDFReader] = {
    log.debug("reading from \'" + data + "\' with format " + format)
    for {
      cs <- getContents(data)
      triples <- RDFAsJenaModel.fromChars(cs, format)
    } yield { log.debug("After reading " + triples); triples }
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

  def convertSchema(opts: Opts): Unit = { // Try[(Result[Typing], PrefixMap)] = {
    verbose("Converting schema")
    val result = Schema.fromFile(opts.schema(), opts.schema_format())
    result match {
      case Success((schema,pm)) => {
        val schemaFormat = opts.outschema_format()
      val schemaOut = schema.serialize(schemaFormat)  

      if (opts.showSchema()) {
        verbose("Schema with format " + schemaFormat)
        println(schemaOut)
      }
      if (opts.outschema_file.isDefined) {
        writeFile(opts.outschema_file(),schemaOut)
      }
      }
      case Failure(e) => {
        println("Exception parsing schema: " + e.getMessage)
      }
    }
  }

  def validateSchema(rdf: RDFReader, opts: Opts): Unit = { 
    val result = for (
      (schema, pm) <- Schema.fromFile(opts.schema(), opts.schema_format())
    ) yield {

      if (opts.showSchema()) {
        val schemaFormat = opts.outschema_format()
        println("Schema with format " + schemaFormat)
        println(schema.serialize(schemaFormat))
      }

      val validator : RDFValidator =
        opts.validator() match {
          case "SHEX3" => {
            ShaclMatcher(schema, rdf)
          }
/*          case "DERIV" => {
            ShExMatcher(schema, rdf, opts.withIncoming(), opts.withAny(), ShapeValidatorWithDeriv)
          }
          case "BACK"  => {
            ShExMatcher(schema, rdf, opts.withIncoming(), opts.withAny(), ShapeValidatorBacktracking)
          } */
          case x => throw MainException(s"Unsupported validator type $x")
        }


      val r =
        if (opts.node.isSupplied)
          if (opts.shape_label.isSupplied)
            validator.match_node_label(IRI(opts.node()))(validator.labelStr(opts.shape_label()))
          else
            validator.match_node_AllLabels(IRI(opts.node()))
        else if (opts.shape_label.isSupplied)
          validator.matchAllNodes_Label(validator.labelStr(opts.shape_label()))
        else
          validator.matchAllNodes_AllLabels
      (r, pm)
    }
    result match {
      case Success((validationResult, pm)) => {
        println(validationResult.show(opts.cut())(pm))
      }
      case Failure(f) => {
        println("Failure: " + f)
      }
    }
  }
  
  def time[A](a: => A) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    println("%d microseconds".format(micros))
    result
  }
  
  def writeFile(name: String, contents: String): Unit = {
    import java.nio.file._
    val path = Paths.get(name)
    Files.write(path, contents.getBytes)
  }
  
}

