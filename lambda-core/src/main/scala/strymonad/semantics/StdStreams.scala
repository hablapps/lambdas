package lambdas
package strymonad
package semantics

import cats.Id

object StdStreams extends Strymonad[Id, Stream, Id] {

  // Producers

  def ofArr[A](data: Array[A]): Stream[A] = ???

  def unfold[S, A](step: S => Option[(S, A)], seed: S): Stream[A] = ???

  // Transformers

  def map[A, B](f: A => B)(str: Stream[A]): Stream[B]                               = ???
  def filter[A](pred: A => Boolean)(str: Stream[A]): Stream[A]                      = ???
  def take[A](count: Int)(str: Stream[A]): Stream[A]                                = ???
  def flatMap[A, B](f: A => Stream[B])(str: Stream[A]): Stream[B]                   = ???
  def zipWith[A, B, C](f: (A, B) => C)(str1: Stream[A], str2: Stream[B]): Stream[C] = ???
  def fold[A, B](nil: B, cons: (B, A) => B)(str: Stream[A]): Stream[B]              = ???

  // Consumer

  def runFold[A, B](nil: B, cons: (B, A) => B)(str: Stream[A]): B = ???

}
