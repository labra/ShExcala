package es.weso.utils

import java.io.File

object FileUtils {

  // Obtained from: http://stackoverflow.com/questions/8323760/java-get-uri-from-filepath
  def filePath2URI(path: String): String = {
    new File(path).toURI().toURL().toExternalForm()
  }

  def writeFile(name: String, contents: String): Unit = {
    import java.nio.file._
    val path = Paths.get(name)
    Files.write(path, contents.getBytes)
  }

}