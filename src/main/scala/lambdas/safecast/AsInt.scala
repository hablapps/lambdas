package lambdas
package safecast

import scalaz.Leibniz._

case class AsInt[A](eq: Option[A === Int]) {

  def as[F[_]](fa: F[A]): Option[F[Int]] =
    eq.map(_.subst[F](fa))
}

object AsInt {

  implicit val AsIntType = new Type[AsInt] {
    def tint: AsInt[Int] =
      AsInt[Int](Option(refl))

    def tarr[T1, T2](t1: AsInt[T1], t2: AsInt[T2]): AsInt[T1 => T2] =
      AsInt[T1 => T2](None)
  }
}
