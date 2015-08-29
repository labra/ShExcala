package es.weso.utils

import scala.annotation.tailrec


object SeqUtils {

  /**
   * zipN(Seq(1,2,3),Seq(4,5),Seq(6,7)) = 
   *   Seq(Seq(1,4,6), Seq(1,4,7), Seq(1,5,6), Seq(1,5,7), Seq(2,4,6),....
   *   
   *   [[3,4]] -> [[Some(3)],[Some(4)]]
   *   [[1,2],[3,4]] -> 
   *     [[Some(1),Some(3)],
   *      [Some(1),Some(4)],
   *      [Some(2),Some(3)],
   *      [Some(2),Some(4)]]
   *   [[0],[1,2],[3,4]] -> 
   *     [[Some(0),Some(1),Some(3)],
   *      [Some(0),Some(1),Some(4)],
   *      [Some(0),Some(2),Some(3)],
   *      [Some(0),Some(2),Some(4)]]
   *   [[0],[1,2],[]] -> [[Some(0),Some(1),None],[Some(0),Some(2),None]]
   *   [[0],[],[1,2]] -> [[Some(0),None,Some(1)],[Some(0),None,Some(2)]]
   */
  def zipN[A](s: Seq[Seq[A]]): Seq[Seq[Option[A]]] = {
    def f(x: Seq[A], rest: Seq[Seq[Option[A]]]): Seq[Seq[Option[A]]] = {
      val none : Option[A] = None 
      if (x.isEmpty) rest.map(s => none +: s)
      else for {
      v <- x
      r <- rest
    } yield Some(v) +: r
    }
    s.foldRight(Seq(Seq[Option[A]]()))(f)
  }

}