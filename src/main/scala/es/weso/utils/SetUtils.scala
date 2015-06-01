package es.weso.utils

import scala.annotation.tailrec


object SetUtils {
  /* pSet s generates the power set of s, pairing each subset with its complement.
     e.g. pSet [1,2] = [([1,2],[]),([1],[2]),([2],[1]),([],[1,2])].
 */
  /* Non tail recursive pSet */
  def pSetOld[A](set: Set[A]): Stream[(Set[A], Set[A])] = {
    if (set.isEmpty) Stream((Set(), Set()))
    else {
      val sets = pSetOld(set.tail)
      val x = set.head
      sets.map(addFirst(x)) ++ sets.map(addSecond(x))
    }
  }

  /* Tail recursive pSet */
  def pSet[A](set: Set[A]): Stream[(Set[A], Set[A])] = {

    @annotation.tailrec
    def pSetRec(set: Set[A],
      acc: Stream[(Set[A], Set[A])]): Stream[(Set[A], Set[A])] = {
      if (set.isEmpty) acc
      else {
        val x = set.head
        pSetRec(set.tail, acc.map(addFirst(x)) ++ acc.map(addSecond(x)))
      }
    }
    pSetRec(set, Stream((Set(), Set())))
  }

  def addFirst[A](x: A)(pair: (Set[A], Set[A])): (Set[A], Set[A]) = {
    (pair._1 + x, pair._2)
  }

  def addSecond[A](x: A)(pair: (Set[A], Set[A])): (Set[A], Set[A]) = {
    (pair._1, pair._2 + x)
  }
}