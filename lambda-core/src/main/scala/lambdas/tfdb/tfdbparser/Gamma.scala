package lambdas
package trees
package tfdbparser

import tfdb._
import syntax._
import cats.syntax.either._

trait Gamma[Γ, E, T[_]] {
  def findVar[P[_, _]: Lambda[T, ?[_, _]]](
      name: String,
      gamma: Γ
  ): Either[String, DynLTerm[T, P, E]]
}

object Gamma {

  case class Var[T[_], A](name: String, typ: T[A])

  implicit def notFound[T[_]]: Gamma[Unit, Unit, T] = new Gamma[Unit, Unit, T] {
    def findVar[P[_, _]: Lambda[T, ?[_, _]]](
        name: String,
        gamma: Unit
    ): Either[String, DynLTerm[T, P, Unit]] =
      Left(s"Var not found: $name")
  }

  implicit def foundVar[Γ, E, T[_]: ArrowType, A](
      implicit G: Gamma[Γ, E, T],
      TA: T[A]
  ): Gamma[(Var[T, A], Γ), (A, E), T] =
    new Gamma[(Var[T, A], Γ), (A, E), T] {

      def findVar[P[_, _]](
          name: String,
          gamma: (Var[T, A], Γ)
      )(implicit L: Lambda[T, P]): Either[String, DynLTerm[T, P, (A, E)]] =
        gamma match {

          case (Var(n, typ), _) if n == name =>
            Right(DynLTerm(typ, vz[T, P, E, A]))

          case (_, tail) =>
            for {
              dt <- G.findVar[P](name, tail)
            } yield DynLTerm(dt.typ, L.vs(dt.term)(dt.typ, TA))
        }
    }
}
