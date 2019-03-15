package lambdas
package trees
package semantics

import syntax._
import safecast._
import interpreters._

import tfdb._
import arithmetic._

case class AdHocTermParser[P[_, _]]()(implicit L: Lambda[P], A: ForAll[P, Arithmetic])
    extends Interpreter[Tree, ParsedLambdaTerm[P]] { self =>
  def apply(tree: Tree) = new ParsedLambdaTerm[P] {
    def apply[Γ, E](γ: Γ)(implicit G: Gamma[Γ, E]) =
      tree match {
        case IntT(i) =>
          Right(DynLTerm(tint[TypeTerm], A[E].int(i)))

        case Add(e1, e2) =>
          for {
            dt1 <- self(e1)(γ)
            dt2 <- self(e2)(γ)
            _dt1 <- dt1
              .as(tint[TypeTerm])
              .toEither(s"First operand of add, not an integer: ${dt1.typ}")
            _dt2 <- dt2.asInt.toEither(s"Second operand of add, not an integer: ${dt2.typ}")
          } yield DynLTerm(tint[TypeTerm], A[E].add(_dt1)(_dt2))

        case Var(name) =>
          G.findVar(name, γ)

        case Lam(name, typ, body) =>
          for {
            ty1 <- TypeParser.apply(typ)
            db <- self(body)
              .apply[(Gamma.Var[ty1.A], Γ), (ty1.A, E)]((Gamma.Var(name, ty1.typ), γ))
          } yield DynLTerm(ty1.typ -> db.typ, L.lam(db.term))

        case App(ft, at) =>
          for {
            df  <- self(ft).apply(γ)
            asA <- df.asArrow.toEither(s"Not a lambda: ${df.typ}")
            da  <- self(at).apply(γ)
            _da <- da.as(asA.typ1).toEither(s"Not argument: ${da.typ}")
          } yield DynLTerm(asA.typ2, L.app(asA.term)(_da))

        case _ =>
          Left(s"ParseTerm error: $tree")
      }
  }
}
