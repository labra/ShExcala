package es.weso.monads

trait Monad[+A] extends Functor[A] {
  type M[+A] <: Monad[A]
  def flatMap[B](f: A => M[B]): M[B]
}

trait Functor[+A] {
  type M[+A] <: Functor[A]
  def map[B](f: A => B): M[B]
}

trait Filter[+A] {
  type M[+A] <: Filter[A]
  def filter(f: A => Boolean): M[A]
}

trait Plus[+A] {
  type M[+A] <: Plus[A]
  def plus[B >: A](other: => M[B]): M[B]
}

trait OrElse[+A] {
  type M[+A] <: OrElse[A]
  def orElse[B >: A](other: => M[B]): M[B]
}

trait Units {
  type M[+A]
  def unit: M[Unit]
  def unit[A](a: => A): M[A]
}

trait Zero {
  type M[+A]
  def zero: M[Nothing]
}

