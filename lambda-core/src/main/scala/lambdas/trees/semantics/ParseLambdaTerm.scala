package lambdas
package trees
package semantics

import syntax._
import safecast._
import interpreters._

import tfdb._

trait LambdaTermInterpreter[P[_, _]] {
  def apply[Γ, E](tree: Tree)(implicit G: Gamma[Γ, E]): Γ => Either[String, DynLTerm[P, E]]
}

object ParseLambdaTerm {

  def apply[P[_, _]](rec: => LambdaTermInterpreter[P])(
      implicit
      L: Lambda[P]
  ) = new LambdaTermInterpreter[P] {
    def apply[Γ, E](tree: Tree)(implicit G: Gamma[Γ, E]) =
      gamma =>
        tree match {

          case Var(name) =>
            G.findVar(name, gamma)

          case Lam(name, typ, body) =>
            for {
              ty1 <- ParseType.apply(typ)
              db <- rec[(Gamma.Var[ty1.A], Γ), (ty1.A, E)](body)
                .apply((Gamma.Var(name, ty1.typ), gamma))
            } yield DynLTerm(ty1.typ -> db.typ, L.lam(db.term))

          case App(ft, at) =>
            for {
              df  <- rec(ft).apply(gamma)
              asA <- df.asArrow.toEither(s"Not a lambda: ${df.typ}")
              da  <- rec(at).apply(gamma)
              _da <- da.as(asA.typ1).toEither(s"Not argument: ${da.typ}")
            } yield DynLTerm(asA.typ2, L.app(asA.term)(_da))

          case _ =>
            Left(s"ParseTerm error: $tree")
        }
  }
}
