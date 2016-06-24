package es.weso.rdf.validator

import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import buildinfo._
import com.typesafe.config._
import es.weso.shex.SchemaFormat
import es.weso.shex.DataFormat

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
    validate = (x => DataFormat.available(x)),
    descr = "Input data format. Available values: " + DataFormat.toString)

  val outdata_format = opt[String]("out-data-format",
    required = false,
    default = Some("TURTLE"),
    descr = "Output data format. Available values: " + DataFormat.toString,
    validate = (x => DataFormat.available(x)),
    noshort = true)

  val mode = opt[String]("mode",
    required = true,
    default = Some(Modes.default),
    descr = s"Validation mode. Available modes: ${Modes.show}",
    validate = x => Modes.lookup(x).isDefined,
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
    descr = "Schema file")

  val schema_format = opt[String]("schema-format",
    short = 'x',
    default = Some(SchemaFormat.default.name),
    descr = "Schema Format. Available values: " + SchemaFormat.toString,
    validate = (x => SchemaFormat.available(x)))

  val schema_version = opt[String]("schema-version",
    short = 'r',
    default = Some(ValidatorVersions.default),
    descr = "Schema Version. Available values: " + ValidatorVersions.toString,
    validate = (x => ValidatorVersions.available(x)))

  val outschema_format = opt[String]("out-schema-format",
    short = 'u',
    required = false,
    default = Some("SHEXC"),
    descr = "Output schema format. Available values: " + SchemaFormat.toString,
    validate = (x => SchemaFormat.available(x)))

  val outschema_file = opt[String]("out-schema",
    short = 'o',
    required = false,
    descr = "Output schema file")

  val showSchema = toggle("showSchema",
    prefix = "no-",
    default = Some(false),
    descrYes = "show schema",
    descrNo = "don't show schema",
    noshort = true)

  val time = toggle("time",
    prefix = "no-",
    default = Some(false),
    descrYes = "show time",
    descrNo = "don't show time",
    short = 't')

  val memory = toggle("memory",
    prefix = "no-",
    default = Some(false),
    descrYes = "show memory used",
    descrNo = "don't show memory used",
    short = 'm')

  val showData = toggle("showData",
    prefix = "no-",
    default = Some(false),
    descrYes = "show input Data",
    descrNo = "don't show input Data",
    noshort = true)

  val cut = opt[Int](
    short = 'c',
    default = Some(1),
    noshort = true,
    validate = (0<))

  val withIncoming = toggle("withIncoming",
    prefix = "no-",
    default = Some(false),
    descrYes = "validates with nodes incoming",
    descrNo = "does not validate nodes incoming",
    noshort = true)

  val withAny = toggle("withAny",
    prefix = "no-",
    default = Some(false),
    descrYes = "adds a node of type any",
    descrNo = "does not add a node of type any",
    noshort = true)

  val withOpen = toggle("withOpen",
    prefix = "no-",
    default = Some(true),
    descrYes = "with open shapes by default",
    descrNo = "with closed shapes by default",
    noshort = true)

  val validator = opt[String](
    default = Some("SHEX3"),
    descr = "Validation algorithm: SHEX3 (Shape Expressions)",
    noshort = true,
    validate = (x => List("SHEX3").contains(x.toUpperCase)))

  val verbose = toggle("verbose",
    prefix = "no-",
    default = Some(false),
    descrYes = "Normal output",
    descrNo = "Verbose output",
    short = 'V')

  val interactive = toggle("interactive",
    prefix = "no-",
    default = Some(false),
    descrYes = "Interactive (stop after each info)",
    descrNo = "Non-interactive",
    short = 'I')

  val version = toggle("version",
    prefix = "no-",
    default = Some(false),
    descrYes = "Show version",
    descrNo = "Don't show version",
    short = 'v')

  val help = toggle("help",
    default = Some(false),
    descrYes = "Show help message",
    descrNo = "Don't show help message",
    short = 'h')

  mutuallyExclusive(data, endpoint)

  override protected def onError(e: Throwable) = onError(e, builder)
}
