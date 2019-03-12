package lambdas
package safecast

import cats.evidence._

abstract class Cast[A] {
  def apply[B](tb: TypeTerm[B]): Option[A Is B]

  def as[B, F[_]](tb: TypeTerm[B], fa: F[A]): Option[F[B]] =
    apply(tb).map(_.substitute[F](fa))
}

object Cast {

  implicit val CastType = new Type[Cast] {

    def tint = new Cast[Int] {
      def apply[B](tb: TypeTerm[B]) =
        tb[CastInt].eq map (_.flip)
    }

    def tarr[T1, T2](t1: Cast[T1], t2: Cast[T2]) = new Cast[T1 => T2] {
      def apply[B](tb: TypeTerm[B]) = {
        val asArr = tb[CastArrow]
        for {
          value <- asArr.eq
          eqT1  <- t1(value._1)
          eqT2  <- t2(value._2)
        } yield (eqT1, eqT2).lift2[Function1].andThen(value._3.flip)
      }
    }
  }
}
