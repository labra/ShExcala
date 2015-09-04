package es.weso.monads

import util.{ Failure => TryFailure }
import util.Success
import util.Try
import es.weso.utils.SetUtils
import io.StdIn._ 
import jline.console._


/**
 * Result class represents the result of a validation process
 * The result can be: 
 * - `Passed` with a list of values
 * - `Failure` with a string message 
 */
sealed abstract class Result[+A] {
  
  def toSingle: Try[A] = {
    this match {
      case Passed(rs) => 
        if (rs.size == 1) Success(rs.head)
        else TryFailure(ResultException("More than one result"))
      case Failure(msg) => TryFailure(ResultException(msg))
    }
  }

  def run(): Try[Stream[A]] = {
    this match {
      case Passed(rs) => Success(rs)
      case Failure(msg) => TryFailure(ResultException(msg))
    }
  }

  def isFailure: Boolean
  def isValid: Boolean = !isFailure
  def failMsg: String

  def toList(): List[A] = {
    this match {
      case Passed(rs) => rs.toList
      case Failure(msg) => List()
    }
  }

  def toList(n: Int): List[A] = {
    this match {
      case Passed(rs) => rs.take(n).toList
      case Failure(msg) => List()
    }
  }

  def noResult[B]: Result[B] = Passed(Stream())

  def appendResult[B](r: Result[B], x: Result[B]): Result[B] = {
    (x, r) match {
      case (Passed(rs1), Passed(rs2)) => Passed(rs1 ++ rs2)
      case (Passed(rs1), Failure(_)) => Passed(rs1)
      case (Failure(_), Passed(rs2)) => Passed(rs2)
      case (Failure(msg1), Failure(msg2)) => Failure(msg1 + "\n" + msg2)
    }
  }

  def concatResults[B](rs: Stream[Result[B]]): Result[B] = {
    rs.foldLeft(noResult: Result[B])(appendResult)
  }

  // TODO: I added this declaration to avoid warning...
  // check if there is a better way to define withFilter
  //  def withFilter = filter _

  def withFilter(p: A => Boolean): Result[A] = {
    this match {
      case Passed(rs) => Passed(rs.filter(p))
      case Failure(msg) => Failure(msg)
    }
  }

  def flatMap[B](f: A => Result[B]): Result[B] = {
    this match {
      case Passed(rs) => concatResults(rs.map(f))
      case Failure(msg) => Failure(msg)
    }
  }

  def map[B](f: A => B): Result[B] = {
    this match {
      case Passed(rs) => Passed(rs.map(f))
      case Failure(msg) => Failure(msg)
    }
  }

  def orelse[B >: A](other: => Result[B]): Result[B] = {
    this match {
      case Passed(rs1) => other match {
        case Passed(rs2) => Passed(rs1 ++ rs2)
        case Failure(_) => Passed(rs1)
      }
      case Failure(msg) => other
    }
  }

  def xor[B >: A](other: => Result[B]): Result[B] = {
    if (this.isFailure) other
    else 
      if (other.isFailure) this
      else 
        Failure("XOr: both branches passed")
  }
  
  def not: Result[Boolean] = {
    this match {
      case Passed(_) => Passed(Stream(false))
      case Failure(_) => Passed(Stream(true))
    }
  }
  
}

case class Passed[+A](passed: Stream[A]) extends Result[A] {
  override def isFailure = passed.isEmpty

  override def failMsg = ""
}

case class Failure(msg: String) extends Result[Nothing] {
  def isFailure = true

  override def failMsg = msg
}

case class ResultException(msg: String) extends RuntimeException(msg)

object Result {
  
  var interactive = true
  var traceOn = false
  val console = new ConsoleReader()

  def unit[A](x: A): Result[A] = Passed(Stream(x))

  def failure(msg: String): Result[Nothing] = Failure(msg)

  def merge[B](comp1: Result[B], comp2: Result[B], combine: (B, B) => B): Result[B] = {
    (comp1, comp2) match {
      case (Passed(rs1), Passed(rs2)) => {
        if (rs1.isEmpty) {
          Passed(rs2)
        } else if (rs2.isEmpty) Passed(rs1)
        else {
          Passed(rs1.map(x => rs2.map(y => combine(x, y))).flatten)
        }
      }
      case (Passed(rs1), Failure(_)) => Passed(rs1)
      case (Failure(_), Passed(rs2)) => Passed(rs2)
      case (Failure(msg1), Failure(msg2)) => Failure(msg1 + "\n" + msg2)
    }
  }

  def combineAll[A, B](
    ls: List[A],
    eval: A => Result[B],
    combine: (B, B) => B): Result[B] = {
    def e: Result[B] = Passed(Stream())
    def step(x: A, r: Result[B]): Result[B] = {
      merge(eval(x), r, combine)
    }
    ls.foldRight(e)(step)
  }

  def passSome[A, B](ls: List[A],
    eval: A => Result[B]): Result[B] = {
    def e: Result[B] = Passed(Stream())
    def step(x: A, r: Result[B]): Result[B] = eval(x) orelse r
    ls.foldRight(e)(step)
  }

  def passAll[A, B](
    ls: List[A],
    current: B,
    eval: (A, B) => Result[B]): Result[B] = {
    ls match {
      case Nil => Passed(Stream(current))
      case x :: xs => eval(x, current).flatMap(next => passAll(xs, next, eval))
    }
  }

  def liftOption[A](opt: Option[A]): Result[A] = {
    opt match {
      case None => Failure("Option with value None")
      case Some(v) => Passed(Stream(v))
    }
  }

  def liftTry[A](t: Try[A]): Result[A] = {
    t match {
      case TryFailure(e) => Failure(e.getMessage())
      case Success(v) => Passed(Stream(v))
    }
  }
  
  def setTrace(toggle: Boolean): Result[Boolean] = {
    traceOn = toggle
    unit(true)
  } 
  
  def trace(msg:String): Result[Boolean] = {
    if (traceOn) {
      println("\nTrace: " + msg)
      if (interactive) {
        println("\nAction? (n = next, r = resume):")
        val next = console.readCharacter()
        next match {
        case 'n' => unit(true) 
        case 'r' => {
          interactive = false
          unit(true)
        }
        case _ => trace(msg)
      } 
    }
    else
      unit(true)
    } else
      unit(false)
  }
  
  def not[A](result: Result[A]): Result[Boolean] = {
    result.not
  }
  
  def xor[A](result1: Result[A], result2: Result[A]): Result[A] = {
    result1.xor(result2)
  }
  
  def anyOf[A](set:Set[A]):Result[(A,Set[A])] = {
    Passed(set.map(x => (x, set - x)).toStream)
  }

  def parts[A](set: Set[A]): Result[(Set[A], Set[A])] = {
    Passed(SetUtils.pSet(set))
  }
  
  def decompose[A](set:Set[A],n:Int): Result[List[Set[A]]] = {
    Passed(SetUtils.decompose(set,n))
  }
  
  

}