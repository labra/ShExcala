package es.weso.rdf.validator

import scala.util.{ Failure, Success, Try }

import org.rogach.scallop.Scallop
import org.rogach.scallop.exceptions.Help

import com.typesafe.config.ConfigFactory

import buildinfo.BuildInfo
import es.weso.rdf.{ PrefixMap, RDFReader }
import es.weso.rdf.jena.{ Endpoint, RDFAsJenaModel }
import es.weso.rdf.nodes.{ IRI, RDFNode }
import es.weso.shex.{ Label, PrefixMaps, Schema, ShEx, ShExMatcher }
import es.weso.utils.FileUtils
import es.weso.utils.IO.getContents
import es.weso.utils.PerformanceUtils.{ getTimeFrom, getTimeNow, showRuntimeMemory, showTime }
import es.weso.utils.Debugging2

object Main extends App with Debugging2 {

  override def main(args: Array[String]): Unit = {

    val conf = ConfigFactory.load()
    val opts = new Opts(args, errorDriver)
    opts.verify()

    if (args.length == 0) {
      opts.printHelp()
      return
    }

    if (opts.version()) {
      println("Show version")
      showVersion()
    }

    setVerbose(opts.verbose())
    setInteractive(opts.interactive())
    debug("Interactive " + opts.interactive())

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

    if (opts.schema.isDefined) {
      if (maybe_rdf.isDefined) {
        val rdf = maybe_rdf.get
        validateSchema(rdf, opts)
      } else {
        convertSchema(opts)
      }
      val micros = getTimeFrom(now)
      val runtime = Runtime.getRuntime()
      if (opts.time()) { showTime(micros) }
      if (opts.memory()) { showRuntimeMemory(runtime) }

    } else { // If no schema...check to validate nodeShape declarations in RDF
      if (maybe_rdf.isDefined) {
        println("Validating only rdf...")
        val attempts = ShEx.validateRDF(maybe_rdf.get)
        showAttempts(attempts, opts.verbose(), cut, PrefixMaps.commonShacl)
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
    debug("Converting schema")
    val result = Schema.fromFile(opts.schema(), opts.schema_format())
    result match {
      case Success((schema, pm)) => {
        val schemaFormat = opts.outschema_format()
        val schemaOut = schema.serialize(schemaFormat)

        if (opts.showSchema()) {
          debug("Schema with format " + schemaFormat)
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
              ShExMatcher(schema, rdf)
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
            println(validator.showResult(r,cut,pm))
          } else {
            val r = validator.match_node_AllLabels(IRI(opts.node()))
            println(validator.showResult(r,cut,pm))
          }
        else if (opts.shape_label.isSupplied) {
          val r = validator.matchAllNodes_Label(validator.labelStr(opts.shape_label()))
          println(validator.showResult(r,cut,pm))
        } else {
          Modes.lookup(opts.mode()) match {
            case Some(Modes.allNodes_allLabels) => {
              println("Validating all nodes - all labels")
              val r = validator.matchAllNodes_AllLabels
              println(validator.showResult(r,cut,pm))
            }
            case Some(Modes.declared) => {
              println("Validating scope declarations")
              val r = validator.validate
              println(validator.showResult(r,cut,pm))
              // println(ValidationAttempt.showAttempts(attempts, opts.verbose(), cut, pm))
            }
            case Some(Modes.allNodes_start) => {
              println("Not implemented yet validation with start")
            }
            case Some(m) => {
              println(s"Unknown mode $m")
            }
            case None => {
              throw MainException(s"Unsupported mode: ${opts.mode()}")
            }
          }
        }
      }
    }

  }

}

