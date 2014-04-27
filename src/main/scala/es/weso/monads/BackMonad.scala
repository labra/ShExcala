package es.weso.monads

import scalaz._
import Scalaz._

sealed abstract class BackMonad[+A] {

  def run(): Stream[A] = {
    this match {
      case Passed(rs) => rs
      case Failure(msg) => throw new BackMonadError(msg)
    }
  }
  
  def isFailure: Boolean

  def noResult[B]: BackMonad[B] = Passed(Stream())
  def appendResult[B](r: BackMonad[B],x:BackMonad[B]): BackMonad[B] = {
    (x,r) match {
      case (Passed(rs1),Passed(rs2)) => Passed(rs1++rs2)
      case (Passed(rs1),Failure(_)) => Passed(rs1)
      case (Failure(_),Passed(rs2)) => Passed(rs2)
      case (Failure(msg1),Failure(msg2)) => Failure(msg1 + "\n" + msg2)
    }
  }
  

  def concatResults[B](rs: Stream[BackMonad[B]]): BackMonad[B] = {
    rs.foldRight(noResult)(???)
  }

  def flatMap[B](f: A => BackMonad[B]): BackMonad[B] = {
    this match {
      case Passed(rs) => concatResults(rs.map(f))
      case Failure(msg) => Failure(msg)
    }
  }

  def map[B](f: A => B) : BackMonad[B] = {
    this match {
      case Passed(rs) => Passed(rs.map(f))
      case Failure(msg) => Failure(msg)
    }
  }

  def orelse[B >: A](other: => BackMonad[B]): BackMonad[B] = {
    this match {
      case Passed(rs1) => other match {
        case Passed(rs2) => Passed(rs1 ++ rs2)
        case Failure(_)  => Passed(rs1)
      }
      case Failure(msg) => other
    }
  } 

 
}

case class Passed[+A](passed: Stream[A]) extends BackMonad[A] {

  def isFailure = passed.isEmpty
  
}
  
case class Failure(msg: String) extends BackMonad[Nothing] {


  def isFailure = true

}

case class BackMonadError(msg: String) extends RuntimeException(msg)


object BackMonad {

  def unit[A](x:A) : BackMonad[A] = Passed(Stream(x))

  def failure(msg:String):BackMonad[Nothing] = Failure(msg)

}