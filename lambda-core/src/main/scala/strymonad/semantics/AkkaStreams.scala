package lambdas
package strymonad
package semantics

import cats.Id, cats.data.Kleisli
import akka.NotUsed
import akka.stream.scaladsl.Source
import akka.stream.Materializer
import scala.concurrent.Future

object AkkaStreams
    extends Strymonad[
      Id,
      Source[?, NotUsed],
      Kleisli[Future, Materializer, ?]
    ] {
  type S[A] = Source[A, NotUsed]
  type O[A] = Kleisli[Future, Materializer, A]

  // Producers

  def ofArr[A](data: Array[A]): S[A] =
    Source.fromIterator(() => data.iterator)

  def unfold[B, A](step: B => Option[(B, A)], seed: B): S[A] =
    Source.unfold(seed)(step)

  // Transformers

  def map[A, B](f: A => B)(str: S[A]): S[B] =
    str.map(f)

  def filter[A](pred: A => Boolean)(str: S[A]): S[A] =
    str.filter(pred)

  def take[A](count: Int)(str: S[A]): S[A] =
    str.take(count.toLong)

  def flatMap[A, B](f: A => S[B])(str: S[A]): S[B] =
    str.flatMapConcat(f)

  def zipWith[A, B, C](f: (A, B) => C)(str1: S[A], str2: S[B]): S[C] =
    str1.zipWith(str2)(f)

  def fold[A, B](nil: B, cons: (B, A) => B)(str: S[A]): S[B] =
    str.fold(nil)(cons)

  // Consumer

  def runFold[A, B](nil: B, cons: (B, A) => B)(str: S[A]): Kleisli[Future, Materializer, B] =
    Kleisli { implicit mat =>
      str.runFold(nil)(cons)
    }

}
