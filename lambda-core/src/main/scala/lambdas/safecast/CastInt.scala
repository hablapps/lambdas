package lambdas
package safecast

import cats.evidence._

case class CastInt[A](eq: Option[A Is Int]) {

  def as[F[_]](fa: F[A]): Option[F[Int]] =
    eq.map(_.substitute[F](fa))
}

object CastInt {

  implicit val CastIntType = new Type[CastInt] {
    def tint: CastInt[Int] =
      CastInt[Int](Option(Is.refl[Int]))

    def tarr[T1, T2](t1: CastInt[T1], t2: CastInt[T2]): CastInt[T1 => T2] =
      CastInt[T1 => T2](None)
  }
}
