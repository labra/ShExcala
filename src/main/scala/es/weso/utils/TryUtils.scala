package es.weso.utils
import util._

object TryUtils {

  // The following code has been taken from this answer: 
  // http://stackoverflow.com/questions/15495678/flatten-scala-try
  // There may be more effcient solutions (but probably less elegant)
  def filterSuccess[A](xs: Seq[Try[A]]): Try[Seq[A]] =
    Try(xs.map(_.get))
    
}

