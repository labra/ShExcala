package es.weso.shex

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
import es.weso.rdfgraph.nodes.IRI
import es.weso.shex.ShapeDoc._
import es.weso.shex.ShapeSyntax._
import es.weso.shex.Typing._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.utils.IO._
import com.hp.hpl.jena.sparql.procedure.library.debug
import buildinfo._

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
    descr = "Data to validate"
  )

  val data_format = opt[String]("in-data-format",
    short = 'f',
    required = false,
    default = Some("TURTLE"),
    validate = (x => DataFormats.available(x)),
    descr = "Input data format. Available values: " + DataFormats.toString
  )

  val outdata_format = opt[String]("out-data-format",
    required = false,
    default = Some("TURTLE"),
    descr = "Output data format. Available values: " + DataFormats.toString,
    validate = (x => DataFormats.available(x)),
    noshort = true
  )

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
    descr = "Schema file")

  val schema_format = opt[String]("schema-format",
    short = 'x',
    default = Some("SHEXC"),
    descr = "Schema Format. Available values: " + SchemaFormats.toString,
    validate = (x => SchemaFormats.available(x))
  )

  val outschema_format = opt[String]("out-schema-format",
    required = false,
    default = Some("SHEXC"),
    descr = "Output schema format. Available values: " + SchemaFormats.toString,
    validate = (x => SchemaFormats.available(x)),
    noshort = true
  )

  val showSchema = toggle("showSchema",
    prefix = "no-",
    default = Some(false),
    descrYes = "show schema",
    descrNo = "don't show schema")

  val time = toggle("time",
    prefix = "no-",
    default = Some(false),
    descrYes = "show time",
    descrNo = "don't time")

  val memory = toggle("memory",
    prefix = "no-",
    default = Some(false),
    descrYes = "show memory used",
    descrNo = "don't show memory used")

  val showData = toggle("showData",
    prefix = "no-",
    default = Some(false),
    descrYes = "show Data",
    descrNo = "don't show Data")

  val cut = opt[Int](
    short = 'c',
    default = Some(1),
    validate = (0<)
  )

  val withIncoming = toggle("withIncoming",
    prefix = "no-",
    default = Some(false),
    descrYes = "validates with nodes incoming",
    descrNo = "does not validate nodes incoming")

  val withAny = toggle("withAny",
    prefix = "no-",
    default = Some(false),
    descrYes = "adds a node of type any",
    descrNo = "does not add a node of type any")

  val withOpen = toggle("withOpen",
    prefix = "no-",
    default = Some(true),
    descrYes = "with open shapes by default",
    descrNo = "with closed shapes by default")

  val validator = opt[String](
    default = Some("DERIV"),
    descr = "Validation algorithm: DERIV (Derivatives), BACK (Backtracking), SPARQL (By SPARQL queries)",
    validate = (x => List("DERIV", "BACK", "SPARQL").contains(x.toUpperCase))
  )

  val verbose = toggle("verbose",
    prefix = "no-",
    default = Some(false),
    descrYes = "Normal output",
    descrNo = "Verbose output")

  val version = opt[Boolean]("version",
    noshort = true,
    descr = "Print version")

  val help = opt[Boolean]("help",
    noshort = true,
    descr = "Show this message")

  mutuallyExclusive(data, endpoint)

  override protected def onError(e: Throwable) = onError(e, builder)
}

object Main extends App with Logging {

  // val logger = Logger(LoggerFactory getLogger "name")

  override def main(args: Array[String]): Unit = {

    val conf = ConfigFactory.load()
    val opts = new Opts(args, errorDriver)
    if (args.length == 0) {
      opts.printHelp()
      return
    }

    if (opts.verbose()) {
      setDebug()
      log.debug("...in debug mode")
    }

    val rdf = opts.data.get match {
      case None => opts.endpoint.get match {
        case None => RDFFromWeb()
        case Some(endpoint) => Endpoint(endpoint)
      }
      case Some(dataFile) => {
        log.debug("Reading from file \'" + dataFile + "\'...")
        val ts = getData(dataFile, opts.data_format()).get
        ts
      }
    }

    if (opts.showData()) {
      println(rdf.serialize(opts.outdata_format()))
    }

    val now = getTimeNow()

    if (opts.schema.isDefined) {
      val result = validateSchema(rdf, opts)
      val micros = getTimeFrom(now)
      val runtime = Runtime.getRuntime()
      result match {
        case Success((typings, pm)) => {
          if (typings.isFailure) {
            println("<No shape typings>")
          } else
            for ((typing, n) <- (typings.run) zip (1 to opts.cut())) {
              println(s"Solution ${n}:\n" + typing.showTyping(pm))
            }
        }
        case Failure(f) => {
          println("Failure: " + f)
        }
      }

      if (opts.time()) { showTime(micros) }
      if (opts.memory()) { showRuntimeMemory(runtime) }

    }
  }

  private def showLabels(schema: Schema): String = {
    schema.getLabels.map(_.getNode.toString ++ " ").mkString
  }

  def getTimeNow(): Long = System.nanoTime
  def getTimeFrom(from: Long): Long = (System.nanoTime - from) / 1000
  def showTime(micros: Long): Unit = {
    println("%d microseconds".format(micros))
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

  def validateSchema(rdf: RDFReader, opts: Opts): Try[(Result[Typing], PrefixMap)] = {
    for (
      (schema, pm) <- Schema.fromFile(opts.schema(), opts.schema_format())
    ) yield {

      log.debug("Got schema. Labels: " + showLabels(schema))
      if (opts.showSchema()) {
        println(schema.serialize(opts.outschema_format()))
      }

      val validator =
        opts.validator() match {
          case "DERIV" => ShapeValidatorWithDeriv
          case "BACK" => ShapeValidatorBacktracking
        }

      val matcher = Matcher(schema, rdf, opts.withIncoming(), opts.withAny(), validator)

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
  }

  def time[A](a: => A) = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    println("%d microseconds".format(micros))
    result
  }
}

