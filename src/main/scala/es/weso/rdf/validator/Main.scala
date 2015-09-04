package es.weso.rdf.validator

import scala.util._
import org.slf4j._
import es.weso.utils.Logging
import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import com.typesafe.config._
import es.weso.monads.Result._
import es.weso.monads.Result
import es.weso.rdf._
import es.weso.rdf.jena._
import es.weso.shex.ShapeDoc._
import es.weso.shex.ShapeSyntax._
import es.weso.shex.ShExMatcher
import es.weso.shex.Typing._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.utils.IO._
import buildinfo._
import es.weso.shacl._
import es.weso.shex.ShapeValidatorWithDeriv
import es.weso.shex.Typing
import es.weso.shex.Schema
import es.weso.shacl.{Schema => SHACL_Schema}
import es.weso.shex.ShapeValidatorBacktracking
import es.weso.shex._
import es.weso.rdfgraph.nodes._
import es.weso.utils.Verbosity

case object ValidatorVersions {
  lazy val validatorVersions = List("SHEX_01", "SHACL_01")

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
    default = Some("DERIV"),
    descr = "Validation algorithm: DERIV (Derivatives), BACK (Backtracking), SPARQL (By SPARQL queries)",
    noshort=true,
    validate = (x => List("DERIV", "BACK", "SPARQL").contains(x.toUpperCase))
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
    
    setVerbosity(opts.verbose())
    verbose("Verbose mode activated")

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

    }
  }

  private def showVersion(): Unit = {
    println("** Version: " + BuildInfo.version)
    println("** Scala version: " + BuildInfo.scalaVersion)
    println("** SBT version: " + BuildInfo.sbtVersion)
  }
  
  private def showLabels(schema: Schema): String = {
    schema.getLabels.map(_.getNode.toString ++ " ").mkString
  }

  def getTimeNow(): Long = System.nanoTime
  
  def getTimeFrom(from: Long): Long = (System.nanoTime - from) / 1000
  
  def showTime(micros: Long): Unit = {
    println("** %d microseconds".format(micros))
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
    val result = SHACL_Schema.fromFile(opts.schema(), opts.schema_format())
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

  def validateSchema(rdf: RDFReader, opts: Opts): Unit = { // Try[(Result[Typing], PrefixMap)] = {
    val result = for (
      (schema, pm) <- Schema.fromFile(opts.schema(), opts.schema_format())
    ) yield {

      log.debug("Got schema. Labels: " + showLabels(schema))
      if (opts.showSchema()) {
        val schemaFormat = opts.outschema_format()
        println("Schema with format " + schemaFormat)
        println(schema.serialize(schemaFormat))
      }

      val validator =
        opts.validator() match {
          case "DERIV" => ShapeValidatorWithDeriv
          case "BACK"  => ShapeValidatorBacktracking
        }

      val matcher = ShExMatcher(schema, rdf, opts.withIncoming(), opts.withAny(), validator)

      val r =
        if (opts.node.isSupplied)
          if (opts.shape_label.isSupplied)
            matcher.matchIRI_Label(IRI(opts.node()))(mkLabel(opts.shape_label()))
          else
            matcher.matchIRI_AllLabels(IRI(opts.node()))
        else if (opts.shape_label.isSupplied)
          matcher.matchAllIRIs_Label(mkLabel(opts.shape_label()))
        else
          matcher.matchAllIRIs_AllLabels()
      (r, pm)
    }
    result match {
      case Success((typings, pm)) => {
        if (typings.isFailure) {
          println("<No shape typings>")
        } else
          for ((typing, n) <- (typings.run.get) zip (1 to opts.cut())) {
            println(s"Solution ${n}:\n" + typing.showTyping(pm))
          }
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

