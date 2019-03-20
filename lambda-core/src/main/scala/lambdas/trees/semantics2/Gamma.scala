package lambdas
package trees
package semantics2

import tfdb._, syntax._

trait Gamma[Γ, E] {
  def findVar[T[_], P[_, _]: Lambda](name: String, gamma: Γ): Either[String, DynLTerm[T, P, E]]
}

object Gamma {

  case class Var[T[_], A](name: String, typ: T[A])

  implicit val notFound: Gamma[Unit, Unit] = new Gamma[Unit, Unit] {
    def findVar[T[_], P[_, _]: Lambda](
        name: String,
        gamma: Unit
    ): Either[String, DynLTerm[T, P, Unit]] =
      Left(s"Var not found: $name")
  }

  implicit def foundVar[Γ, E, A](implicit G: Gamma[Γ, E]): Gamma[(Var[T, A], Γ), (A, E)] =
    new Gamma[(Var[T, A], Γ), (A, E)] {

      def findVar[T[_], A, P[_, _]: Lambda](
          name: String,
          gamma: (Var[T, A], Γ)
      ): Either[String, DynLTerm[T, P, (A, E)]] =
        gamma match {

          case (Var(_, typ), _) =>
            Right(DynLTerm(typ, vz[P, E, T]))

          case (_, tail) =>
            for {
              dt <- G.findVar(name, tail).right
            } yield DynLTerm(dt.typ, vs(dt.term))
        }
    }
}
