package es.weso.rdf.validator

import scala.util._
import org.slf4j._
import es.weso.utils.Logging
import org.rogach.scallop._
import org.rogach.scallop.exceptions._
import buildinfo._
import com.typesafe.config._
import es.weso.rdf._
import es.weso.rdf.jena._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.utils.IO._
import es.weso.shacl._
import es.weso.rdf.nodes._
import es.weso.utils.Verbosity
import es.weso.shacl.Shacl._
import es.weso.utils.FileUtils
import es.weso.utils.PerformanceUtils._

object Main extends App with Verbosity {

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
        println("Validating only rdf...")
        val attempts = Shacl.validateRDF(maybe_rdf.get)
        showAttempts(attempts, isVerbose, cut, PrefixMaps.commonShacl)
      }

    }
  }

  private def showVersion(): Unit = {
    println("** Version: " + BuildInfo.version)
    println("** Scala version: " + BuildInfo.scalaVersion)
    println("** SBT version: " + BuildInfo.sbtVersion)
  }


 def showAttempts(attempts: Try[Seq[ValidationAttempt[RDFNode, Label]]], 
      verbose: Boolean, 
      cut: Int, 
      pm: PrefixMap): Unit = {
    attempts match {
      case Failure(e) => 
        println("Exception trying to validate RDF: " + e)
      case Success(as) => {
        println(ValidationAttempt.showAttempts(as, verbose, cut, pm))
      }
    }
  } 

  /*
  def showAttempt(attempt: ValidationAttempt[RDFNode, Label], 
      verbose: Boolean, 
      cut: Int, 
      pm: PrefixMap): Unit = {
    attempt.show(verbose, cut, pm)
  } */

  private def getData(data: String, format: String): Try[RDFReader] = {
    log.debug("reading from \'" + data + "\' with format " + format)
    for {
      cs <- getContents(data)
      triples <- RDFAsJenaModel.fromChars(cs, format)
    } yield { log.debug("After reading " + triples); triples }
  }

  private def errorDriver(e: Throwable, scallop: Scallop): Nothing = { 
    e match {
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

  def convertSchema(opts: Opts): Unit = { // Try[(Result[Typing], PrefixMap)] = {
    verbose("Converting schema")
    val result = Schema.fromFile(opts.schema(), opts.schema_format())
    result match {
      case Success((schema, pm)) => {
        val schemaFormat = opts.outschema_format()
        val schemaOut = schema.serialize(schemaFormat)

        if (opts.showSchema()) {
          verbose("Schema with format " + schemaFormat)
          println(schemaOut)
        }
        if (opts.outschema_file.isDefined) {
          FileUtils.writeFile(opts.outschema_file(), schemaOut)
        }
      }
      case Failure(e) => {
        println("Exception parsing schema: " + e.getMessage)
      }
    }
  }

  def validateSchema(rdf: RDFReader, opts: Opts): Unit = {
    val schema = opts.schema()
    val schema_format = opts.schema_format()
    val trySchema = Schema.fromFile(schema, schema_format)
    trySchema match {
      case Failure(e) => println(s"Failure parsing schema $schema with format $schema_format: ${e}")
      case Success((schema, pm)) => {

        val cut = opts.cut()

        if (opts.showSchema()) {
          val schemaFormat = opts.outschema_format()
          println("Schema with format " + schemaFormat)
          println(schema.serialize(schemaFormat))
        }

        val validator: RDFValidator =
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

        if (opts.node.isSupplied)
          if (opts.shape_label.isSupplied) {
            val r = validator.match_node_label(IRI(opts.node()))(validator.labelStr(opts.shape_label()))
            println(r.show(cut)(pm))
          } else {
            val r = validator.match_node_AllLabels(IRI(opts.node()))
            println(r.show(cut)(pm))
          }
        else if (opts.shape_label.isSupplied) {
          val r = validator.matchAllNodes_Label(validator.labelStr(opts.shape_label()))
          println(r.show(cut)(pm))
        } else {
          opts.mode() match {
            case "ALL_NODESLABELS" => {
              val r = validator.matchAllNodes_AllLabels
              println(r.show(cut)(pm))
            }
            case "DECLARED" => {
              println("Validating...")
              val r = validator.validate
              println(r.show(cut)(pm))
              // println(ValidationAttempt.showAttempts(attempts, opts.verbose(), cut, pm))
            }
            case str => {
              throw MainException(s"Unsupported mode: $str")
            }
          }
        }
      }
    }

  }

  def printResult(result: ValidationResult[RDFNode, Label, Throwable], cut: Int, pm: PrefixMap): Unit = {
    println(result.show(cut)(pm))
  }



}

