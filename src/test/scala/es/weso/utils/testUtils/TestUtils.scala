package es.weso.utils.testUtils

import org.scalatest._
import util._
import argonaut._, Argonaut._
import java.io.File

trait TestUtils extends FunSpec {
  
  def trying[A](msg: String, t: Try[A]): Try[A] = {
    if (t.isSuccess) t
    else {
      info(s"Failing $msg: ${t}")
      t
    }
  }
  
  def file2json(file: File): Try[Json] = {
    Try {
      val contents = io.Source.fromFile(file)("UTF-8")
      val parsed = JsonParser.parse(contents.mkString)
      parsed.fold(_ =>
        throw new Exception(s"Error parsing ${file.getName()}: $parsed"),
        x => x)
    }
  }

    def jsonFile(file: File): Try[Json] = {
    Try {
      val contents = io.Source.fromFile(file)("UTF-8").mkString
      val parsed = Parse.parse(contents)
      parsed.fold(_ =>
        throw new Exception(s"Error parsing ${file.getName()}: $parsed"),
        x => x)
    }
  }

  // method borrowed from: Alvin Alexander's Scala cookbook 
  def getFilesFromFolder(path: String): List[(File)] = {
    val d = new File(path)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }
  def parseFolderFiles[A](
    folder: String,
    parser: File => Try[A]): List[(File, A)] = {
    val files = getFilesFromFolder(folder)
    files.map(file => {
      val t = parser(file)
      if (t.isFailure) {
        println(s"Failure with file ${file.getName}: $t")
      }
      t.map(a => (file, a))
    }).filter(_.isSuccess).map(_.get)
  }

  def nameWithoutExtension(file: File): String = {
    val name = file.getName
    if (name.contains('.'))
      name.substring(0, name.lastIndexOf('.'))
    else
      name
  }

  def lookupFileWithSameName(file: File, otherPath: String, extension: String): Try[File] = {
    Try {
      val newFileName = otherPath + "/" + nameWithoutExtension(file) + "." + extension
      val d = new File(newFileName)
      if (d.exists && d.isFile) d
      else throw new Exception(s"File: $newFileName not found")
    }
  }

    def jsonsEqual(json1: Json, json2: Json): Unit = {
    if (json1 == json2) {
    } else {
      fail(s"JSons are different \nFrom File:${json1.spaces2}\nFrom ShEx:${json2.spaces2}")
    }
  }

  
}
