package es.weso.utils.testUtils

import org.scalatest._
import util._
import argonaut._, Argonaut._
import Json._
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

  def getFilesFromFolderWithExt(path: String, ext: String): List[(File)] = {
    val d = new File(path)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter((x) => x.isFile && extension(x.getName) == ext).toList
    } else {
      List[File]()
    }
  }

  def extension(str: String): String = {
    if (str contains '.') {
      str.reverse.takeWhile(_ != '.').reverse
    } else ""
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

  def parseFolderFilesWithExt[A](
    folder: String,
    parser: File => Try[A],
    ext: String): List[(File, A)] = {
    val files = getFilesFromFolderWithExt(folder, ext)
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
      fail(s"JSons are different. Diff = ${diff(json1, json2)}")  // \nJson1:${json1.spaces2}\nJson2:${json2.spaces2}.
    }
  }

  def diff(json1: Json, json2: Json): String = {
    json1.fold(
      checkNull(json2),
      checkBool(json2),
      checkNumber(json2),
      checkString(json2),
      checkArray(json2),
      checkObject(json2))
  }

  def checkNull(json: Json): String = {
    if (json.isNull) ""
    else s"json $json should be null"
  }

  def checkBool(json: Json)(v: Boolean): String = {
    if (json.isBool) {
      if (json.bool.get == v) {
        ""
      } else
        s"json $json should be bool $v"
    } else s"json $json should be bool $v"
  }

  def checkNumber(json: Json)(v: JsonNumber): String = {
    if (json.isNumber) {
      if (json.number.get == v) {
        ""
      } else
        s"json $json should be number $v"
    } else s"json $json should be numer $v"
  }
  def checkString(json: Json)(v: String): String = {
    if (json.isString) {
      if (json.string.get == v) {
        ""
      } else
        s"json $json should be string $v"
    } else s"json $json should be string $v"
  }

  def checkArray(json: Json)(v: JsonArray): String = {
    if (json.isArray) {
      val array = json.array.get
      if (array == v) {
        ""
      } else {
        diffArrays(array, v)
      }

    } else s"json $json should be array $v"
  }
  
  def checkObject(json: Json)(v: JsonObject): String = {
    if (json.isObject) {
      val obj = json.obj.get
      if (obj == v) {
        ""
      } else {
        s"Objects different: $obj != $v\n${diffObjects(obj, v)}"
      }
    } else 
      s"json $json should be object to be able to compare with $v"
  }

  def diffObjects(o1: JsonObject, o2: JsonObject): String = {
    val zero = ""
    def cont: ((JsonField, Json), String) => String = { (pair, rest) =>
      val (field, value) = pair
      o1(field) match {
        case None    => s"obj1 $o1 doesn't contain field $field with value $value\n" + rest
        case Some(v) => diff(value, v) + rest
      }
    }
    o2.toMap.foldRight(zero)(cont)
  }

  def diffArrays(o1: JsonArray, o2: JsonArray): String = {
    if (o1.length == o2.length) {
      val zero = ""
      def cont: (((Json, Json), Int), String) => String = { (t, rest) =>
        val ((v1, v2), n) = t
        if (v1 == v2) rest
        else s"Array diff at index $n\n${diff(v1, v2)}\n$rest"
      }
      (o1.toList zip o2.toList zip (1 to o1.length)).foldRight(zero)(cont)
    } else {
      s"Arrays have different lengths: ${o1.length}!=${o2.length}"
    }
  }
}
