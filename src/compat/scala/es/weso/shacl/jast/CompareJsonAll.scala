package es.weso.shacl.jast

import com.typesafe.config._

import java.io.File

import argonaut._, Argonaut._

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import scala.util.{ Try, Success => TrySuccess, Failure => TryFailure }

import scala.io._

import org.scalatest.FunSpec
import org.scalatest._

import scalaz._, Scalaz._
import es.weso.shacl.jast.AST._
import es.weso.shacl._

class CompareJsonsAll extends Driver {
  
  
  describe("Run positive JSON tests converting ShexC -> Schema -> AST -> JSON = JSON") {
    val parsedSchemas = getParsedSchemas(schemasFolder)
    for ((file, json) <- parsedSchemas) {
      it(s"Should handle ${file.getName}") {
//        testComparingJsons(file)
      }
    }
  }
}