package es.weso.monads

import util.{Failure => TryFailure}
import util.Success
import util.Try
import scala.annotation.tailrec

sealed abstract class Result[+A] {

  def run(): Stream[A] = {
    this match {
      case Passed(rs) => rs
      case Failure(msg) => throw new ResultException(msg)
    }
  }
  
  def isFailure: Boolean
  def isValid: Boolean = !isFailure
  def failMsg : String
  
  def toList(): List[A] = {
    this match {
      case Passed(rs) => rs.toList
      case Failure(msg) => List()
    }
  }

  def noResult[B]: Result[B] = Passed(Stream())
  
  def appendResult[B](r: Result[B],x:Result[B]): Result[B] = {
    (x,r) match {
      case (Passed(rs1),Passed(rs2)) => Passed(rs1++rs2)
      case (Passed(rs1),Failure(_)) => Passed(rs1)
      case (Failure(_),Passed(rs2)) => Passed(rs2)
      case (Failure(msg1),Failure(msg2)) => Failure(msg1 + "\n" + msg2)
    }
  }
  
  def concatResults[B](rs: Stream[Result[B]]): Result[B] = {
    rs.foldLeft(noResult: Result[B])(appendResult)
  }

  // TODO: I added this declaration to avoid warning...
  // check if there is a better way to define withFilter
  def withFilter = filter _

  def filter(p: A => Boolean): Result[A] = {
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

  def map[B](f: A => B) : Result[B] = {
    this match {
      case Passed(rs) => Passed(rs.map(f))
      case Failure(msg) => Failure(msg)
    }
  }
  

  def orelse[B >: A](other: => Result[B]): Result[B] = {
    this match {
      case Passed(rs1) => other match {
        case Passed(rs2) => Passed(rs1 ++ rs2)
        case Failure(_)  => Passed(rs1)
      }
      case Failure(msg) => other
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

  def unit[A](x:A) : Result[A] = Passed(Stream(x))

  def failure(msg:String):Result[Nothing] = Failure(msg)

  def merge[B](comp1: Result[B], comp2: Result[B], combine:(B,B)=>B) : Result[B] = {
    (comp1,comp2) match {
      case (Passed(rs1),Passed(rs2)) => {
        if (rs1.isEmpty) {
          Passed(rs2)
        } else if (rs2.isEmpty) Passed(rs1)
        	   else {
        		   Passed(rs1.map(x => rs2.map(y => combine(x,y))).flatten)
        	   }
      }
      case (Passed(rs1),Failure(_)) => Passed(rs1)
      case (Failure(_),Passed(rs2)) => Passed(rs2)
      case (Failure(msg1),Failure(msg2)) => Failure(msg1 + "\n" + msg2)
    }
  }

  def combineAll[A,B](
      ls:List[A], 
      eval:A => Result[B],
      combine:(B,B)=>B): Result[B] = {
    def e : Result[B] = Passed(Stream())
    def step(x:A, r: Result[B]):Result[B] = {
      merge(eval(x),r,combine)
    }
    ls.foldRight(e)(step)
  } 

  
  def passSome[A,B](ls:List[A], 
      eval: A => Result[B]): Result[B] = {
    def e : Result[B] = Passed(Stream())
    def step(x:A,r:Result[B]):Result[B] = eval(x) orelse r
    ls.foldRight(e)(step)
  }

  def passAll[A,B](
      ls:List[A], 
      current: B, 
      eval: (A,B) => Result[B]): Result[B] = {
    ls match {
      case Nil => Passed(Stream(current))
      case x :: xs => eval(x,current).flatMap(next => passAll(xs,next,eval))
    }
  }
  
  def liftOption[A](opt:Option[A]):Result[A] = {
    opt match {
      case None => Failure("Option with value None")
      case Some(v) => Passed(Stream(v))
    }
  }

  def liftTry[A](t:Try[A]):Result[A] = {
    t match {
      case TryFailure(e) => Failure(e.getMessage())
      case Success(v) => Passed(Stream(v))
    }
  }

  def parts[A](set: Set[A]): Result[(Set[A],Set[A])] = {
    Passed(pSet(set))
  }
 
 
  
 /* pSet s generates the power set of s, pairing each subset with its complement.
     e.g. pSet [1,2] = [([1,2],[]),([1],[2]),([2],[1]),([],[1,2])].
 */
  /* Non tail recursive pSet */
  def pSetOld[A](set:Set[A]):Stream[(Set[A],Set[A])] = {
   if (set.isEmpty) Stream((Set(),Set()))
   else { 
     val sets = pSetOld(set.tail)
     val x = set.head
     sets.map(addFirst(x)) ++ sets.map(addSecond(x))
   }
  }
  
  /* Tail recursive pSet */
  def pSet[A](set:Set[A]):Stream[(Set[A],Set[A])] = {
    
   @annotation.tailrec
   def pSetRec(set:Set[A], 
               acc: Stream[(Set[A],Set[A])]): Stream[(Set[A],Set[A])] = {
    if (set.isEmpty) acc
    else { 
     val x = set.head
     pSetRec(set.tail, acc.map(addFirst(x)) ++ acc.map(addSecond(x)))
    }
   }  
   pSetRec(set,Stream((Set(),Set())))
  }

  def addFirst[A](x:A)(pair:(Set[A],Set[A])):(Set[A],Set[A]) = {
    (pair._1 + x, pair._2)
  }

  def addSecond[A](x:A)(pair:(Set[A],Set[A])):(Set[A],Set[A]) = {
    (pair._1, pair._2 + x)
  }
}