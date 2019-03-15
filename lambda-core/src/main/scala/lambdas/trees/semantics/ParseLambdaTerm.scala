package lambdas
package trees
package semantics

import syntax._
import safecast._
import interpreters._

import tfdb._

trait ParsedLambdaTerm[P[_, _]] {
  def apply[Γ, E](γ: Γ)(implicit G: Gamma[Γ, E]): Either[String, DynTerm[P[E, ?]]]
}

object ParsedLambdaTerm {

  case class Parser[P[_, _]]()(implicit L: Lambda[P])
      extends OpenInterpreter[Tree, ParsedLambdaTerm[P]] {
    def apply(rec: => Interpreter[Tree, ParsedLambdaTerm[P]]) =
      (tree: Tree) =>
        new ParsedLambdaTerm[P] {
          def apply[Γ, E](γ: Γ)(implicit G: Gamma[Γ, E]) =
            tree match {
              case Var(name) =>
                G.findVar(name, γ)

              case Lam(name, typ, body) =>
                for {
                  ty1 <- TypeParser.apply(typ)
                  db <- rec(body)
                    .apply[(Gamma.Var[ty1.A], Γ), (ty1.A, E)]((Gamma.Var(name, ty1.typ), γ))
                } yield DynLTerm(ty1.typ -> db.typ, L.lam(db.term))

              case App(ft, at) =>
                for {
                  df  <- rec(ft).apply(γ)
                  asA <- df.asArrow.toEither(s"Not a lambda: ${df.typ[ShowP]}")
                  da  <- rec(at).apply(γ)
                  _da <- da
                    .as(asA.typ1)
                    .toEither(s"Wrong type: ${da.typ[ShowP]} should be ${asA.typ1[ShowP]}")
                } yield DynLTerm(asA.typ2, L.app(asA.term)(_da))

              case _ =>
                Left(s"ParseTerm error: $tree")
            }
        }
  }
}
