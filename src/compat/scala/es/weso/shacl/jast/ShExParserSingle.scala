package es.weso.shacl.jast

import com.typesafe.config._

import java.io.File

import argonaut._, Argonaut._

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import scala.util.{ Try, Success => TrySuccess, Failure => TryFailure }

import scala.io._
import scalaz.\/

import org.scalatest.FunSpec
import org.scalatest._

import scalaz._, Scalaz._
import es.weso.shacl.jast.AST._
import es.weso.shacl._

class ShExParserSingle extends Driver {

  describe("Run specific JSON test") {
    val name = "0"
    val file = new File(schemasFolder + "/" + name + ".json")
    it(s"should pass JSON comparisons on file $name") {
      testComparingJsons(file,true)
    }
    it(s"should pass Schema comparisons on file $name") {
      testComparingSchemas(file)
    }
  }


}
