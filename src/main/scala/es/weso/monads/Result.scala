package es.weso.monads

sealed abstract class Result[+A] {

  def run(): Stream[A] = {
    this match {
      case Passed(rs) => rs
      case Failure(msg) => throw new ResultException(msg)
    }
  }
  
  def isFailure: Boolean

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

  def parts[A](set: Set[A]): Result[(Set[A],Set[A])] = {
    ???
  }
  
  /* pSet s generates the power set of s, pairing each subset
     with its complement.
     e.g. pSet [1,2] = [([1,2],[]),([1],[2]),([2],[1]),([],[1,2])].
     This piece of code has been inspired from: Brent Yorgey: Generating Multiset Partitions
     http://www.haskell.org/wikiupload/d/dd/TMR-Issue8.pdf
*/
  def pSet[A](ls:List[A]):List[(List[A],List[A])] = {
   // def mapx[A]()= 
   def first[A,B,C](f: A => C, pair: (A,B)):(C,B) = (f(pair._1),pair._2)
   def second[A,B,C](f: B => C, pair: (A,B)):(A,C) = (pair._1,f(pair._2))
   
   ls match {
     case Nil => List((List(),List()))
     case x :: xs => ??? // mapx first ++ mapx second
   }  
  }
  /*
    pSet [] = [([],[])]
    pSet (x:xs) = mapx first ++ mapx second where
      mapx which = map (which (x:)) $ pSet xs
      first f  (x,y) = (f x, y)
      second f (x,y) = (x, f y)
  */
}