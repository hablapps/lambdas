package lambdas
package trees
package semantics2

import tfdb._, syntax._

trait Gamma[Γ, E, T[_]] {
  def findVar[P[_, _]: Lambda](name: String, gamma: Γ): Either[String, DynLTerm[T, P, E]]
}

object Gamma {

  case class Var[T[_], A](name: String, typ: T[A])

  implicit def notFound[T[_]]: Gamma[Unit, Unit, T] = new Gamma[Unit, Unit, T] {
    def findVar[P[_, _]: Lambda](
        name: String,
        gamma: Unit
    ): Either[String, DynLTerm[T, P, Unit]] =
      Left(s"Var not found: $name")
  }

  implicit def foundVar[Γ, E, T[_], A](
      implicit G: Gamma[Γ, E, T]
  ): Gamma[(Var[T, A], Γ), (A, E), T] =
    new Gamma[(Var[T, A], Γ), (A, E), T] {

      def findVar[P[_, _]: Lambda](
          name: String,
          gamma: (Var[T, A], Γ)
      ): Either[String, DynLTerm[T, P, (A, E)]] =
        gamma match {

          case (Var(_, typ), _) =>
            Right(DynLTerm(typ, vz[P, E, A]))

          case (_, tail) =>
            for {
              dt <- G.findVar[P](name, tail)
            } yield DynLTerm(dt.typ, vs(dt.term))
        }
    }
}
