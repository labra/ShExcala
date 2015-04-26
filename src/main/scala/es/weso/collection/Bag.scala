package es.weso.collection

import scala.collection.SortedMap

trait Bag[A] {

  def multiplicity(elem: A): Int

  def contains(elem: A): Boolean

  def insert(elem: A): Bag[A]

  def delete(elem: A): Bag[A]

  def elems: Iterator[(A, Int)]

  /**
   * add an element n times to a bag
   * This default implementation is not efficient, it should be overridden
   */
  def add(elem: A, n: Int): Bag[A] =
    (1 to n).toList.foldLeft(this)((s, _) => s.insert(elem))

  def union(other: Bag[A]): Bag[A] =
    other.elems.toList.foldLeft(this)((s, p) => s.add(p._1, p._2))

  def intersection(other: Bag[A]): Bag[A] = ???

  /**
   * Size returns the total number of elements.
   * Notice that if an element appears 4 times, it adds 4 to the counter
   */
  def size: Int = {
    elems.foldLeft(0)((r, n) => r + n._2)
  }

  override def equals(other: Any): Boolean = {
    other.isInstanceOf[Bag[A]] &&
      this.asSortedMap.equals(other.asInstanceOf[Bag[A]].asSortedMap)
  }

  def asSortedMap: SortedMap[A, Int]

  def from(t: Traversable[A]): Bag[A] = {
    t.foldLeft(this)((s, a) => s.insert(a))
  }
}

object Bag {
  def empty[A: Ordering]: Bag[A] =
    BagSortedMap(SortedMap[A, Int]())

  def toBag[A: Ordering](t: Traversable[A]): Bag[A] = {
    empty.from(t)
  }
}