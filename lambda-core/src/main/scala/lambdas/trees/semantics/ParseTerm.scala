package lambdas
package trees
package semantics

import syntax._
import safecast._

import tfdb._

case class ParseTerm[P[_, _], Γ, E](gamma: Γ)(
    implicit
    L: Lambda[P],
    G: Gamma[Γ, E]
) extends Semantics[Either[String, DynLTerm[P, E]]] {

  def apply(tree: Tree) = tree match {

    case IntT(i) =>
      Right(DynLTerm(tint[TypeTerm], L.int(i)))

    case Add(e1, e2) =>
      for {
        dt1  <- apply(e1)
        dt2  <- apply(e2)
        _dt1 <- dt1.as(tint[TypeTerm]).toEither(s"First operand of add, not an integer: ${dt1.typ}")
        _dt2 <- dt2.asInt.toEither(s"Second operand of add, not an integer: ${dt2.typ}")
      } yield DynLTerm(tint[TypeTerm], L.add(_dt1, _dt2))

    case Var(name) =>
      G.findVar(name, gamma)

    case Lam(name, typ, body) =>
      for {
        ty1 <- ParseType(typ)
        db  <- ParseTerm((Gamma.Var(name, ty1.typ), gamma)).apply(body)
      } yield DynLTerm(ty1.typ -> db.typ, L.lam(db.term))

    case App(ft, at) =>
      for {
        df  <- apply(ft)
        asA <- df.asArrow.toEither(s"Not a lambda: ${df.typ}")
        da  <- apply(at)
        _da <- da.as(asA.typ1).toEither(s"Not argument: ${da.typ}")
      } yield DynLTerm(asA.typ2, L.app(asA.term)(_da))

    case _ =>
      Left(s"ParseTerm error: $tree")
  }
}
