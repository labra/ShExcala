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

class RunParsedSingle extends Driver {
  
  describe("Run specific JSON test") {
    val name = "1val1iriStem"
    val file = new File(schemasFolder + "/" + name + ".json")
    it (s"should pass file $name") {
     testComparingJsons(file, true) 
    }
  }

 
}
