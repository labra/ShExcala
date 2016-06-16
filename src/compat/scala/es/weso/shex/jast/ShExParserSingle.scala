package es.weso.shex.jast

import com.typesafe.config._

import java.io.File

import argonaut._, Argonaut._

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import scala.util.{ Try, Success => TrySuccess, Failure => TryFailure }

import scala.io._
import org.scalatest.FunSpec
import org.scalatest._

import es.weso.shex.jast.AST._
import es.weso.shex._

class ShExParserSingle extends Driver {

  describe("Run specific JSON test") {
    val name = "open3groupdotclosecard23Annot3Code2"
    val file = new File(schemasFolder + "/" + name + ".json")
    it(s"should pass JSON comparisons on file $name") {
      testComparingJsons(file,true)
    }
    it(s"should pass Schema comparisons on file $name") {
      testComparingSchemas(file,true)
    }
  }


}
