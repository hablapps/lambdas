package lambdas
package deserialization

import taglessfinal.debruijn._

trait Gamma[Γ, E]{
  def findVar[P[_, _]: Lambda](name: String, gamma: Γ): Either[String, DynTerm[P, E]]
}

object Gamma{

  case class VarDesc[T](name: String, typ: TQ[T])

  implicit val notFound: Gamma[Unit, Unit] = new Gamma[Unit, Unit]{
    def findVar[P[_, _]: Lambda](
        name: String,
        gamma: Unit): Either[String, DynTerm[P, Unit]] =
      Left(s"Var not found: $name")
  }

  implicit def varGamma[Γ, E, T](implicit G: Gamma[Γ, E]): Gamma[(VarDesc[T], Γ), (T, E)] =
    new Gamma[(VarDesc[T], Γ), (T, E)]{
      def findVar[P[_, _]: Lambda](
          name: String,
          gamma: (VarDesc[T], Γ)): Either[String, DynTerm[P, (T, E)]] =
        gamma match {
          case (VarDesc(name, typ), _) =>
            Right(DynTerm(typ, vz[P, E, T]))
          case (_, tail) => for {
            dt <- G.findVar(name, tail).right
          } yield DynTerm(dt.typ, vs(dt.term))
        }
    }
}
