package es.weso.utils

import jline.console.ConsoleReader
import org.slf4j._

/**
  * Trait for debugging
  */
trait Debugging2 {

  var usingLogger     = false
  var usingPrintln    = false
  var log             = LoggerFactory.getLogger("Debugging")
  var interactive     = false
  val console         = new ConsoleReader()

  def setVerbose(verbose: Boolean): Unit = {
    usingPrintln = verbose
  }

  def setInteractive(inter: Boolean): Unit = {
    interactive = inter
  }

  def debug(msg: String) {
    if (usingLogger) {
      log.debug(msg)
    }
    if (usingPrintln) {
      println(msg)
    }
    if (interactive) {
      println("\nAction? (n = next, r = resume):")
      val next = console.readCharacter()
      next match {
        case 'n' => return
        case 'r' => {
          interactive = false
          return
        }
        case c => {
          println(s"\nUnknown char: $c");
          debug(msg)
        }
      }
    }
  }

}