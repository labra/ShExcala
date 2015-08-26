package es.weso.utils

import scala.annotation.tailrec


object SeqUtils {

  /**
   * zipN(Seq(1,2,3),Seq(4,5),Seq(6,7)) = 
   *   Seq(Seq(1,4,6), Seq(1,4,7), Seq(1,5,6), Seq(1,5,7), Seq(2,4,6),....
   *   
   *   [[3,4]] -> [[3],[4]]
   *   [[1,2],[3,4]] -> [[1,3],[1,4],[2,3],[2,4]]
   *   [[0],[1,2],[3,4]] -> [[0,1,3],[0,1,4],[0,2,3],[0,2,4]]
   */
  def zipN[A](s: List[List[A]]): List[List[A]] = {
    def f(x: List[A], rest: List[List[A]]): List[List[A]] = for {
      v <- x
      r <- rest
    } yield v :: r
    s.foldRight(List(List[A]()))(f)
  }

}