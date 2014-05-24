package es.weso.monads

sealed abstract class Result[+A] {

  def run(): Stream[A] = {
    this match {
      case Passed(rs) => rs
      case Failure(msg) => throw new ResultException(msg)
    }
  }
  
  def isFailure: Boolean
  def isValid: Boolean = !isFailure
  
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
    // TODO: substitute by foldLeft
    if (rs.isEmpty) noResult
    else appendResult(rs.head,concatResults(rs.tail))
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
  def isFailure = passed.isEmpty
}
  
case class Failure(msg: String) extends Result[Nothing] {
  def isFailure = true
}

case class ResultException(msg: String) extends RuntimeException(msg)


object Result {

  def unit[A](x:A) : Result[A] = Passed(Stream(x))

  def failure(msg:String):Result[Nothing] = Failure(msg)

  def combineAll[A,B](
      ls:List[A], 
      current: B,
      eval:(A,B) => Result[B]): Result[B] = {
    ls match {
      case Nil => Passed(Stream(current))
      case x :: xs => eval(x,current).flatMap(next => combineAll(xs,next,eval)) 
    }
  }

  
  def passSome[A,B](ls:List[A], eval: A => Result[B]): Result[B] = {
    ls match {
      case Nil => Passed(Stream())
      case x :: xs => eval(x) orelse passSome(xs,eval)
    }
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

  def parts[A](set: Set[A]): Result[(Set[A],Set[A])] = {
    Passed(pSet(set))
  }
 
  
  /* pSet s generates the power set of s, pairing each subset with its complement.
     e.g. pSet [1,2] = [([1,2],[]),([1],[2]),([2],[1]),([],[1,2])].
*/
  def pSet[A](set:Set[A]):Stream[(Set[A],Set[A])] = {
   if (set.isEmpty) Stream((Set(),Set()))
   else { 
     val sets = pSet(set.tail)
     val x = set.head
     sets.map(addFirst(x)) ++ sets.map(addSecond(x))
   }
  }
  
  def addFirst[A](x:A)(pair:(Set[A],Set[A])):(Set[A],Set[A]) = {
    (pair._1 + x, pair._2)
  }

  def addSecond[A](x:A)(pair:(Set[A],Set[A])):(Set[A],Set[A]) = {
    (pair._1, pair._2 + x)
  }
}