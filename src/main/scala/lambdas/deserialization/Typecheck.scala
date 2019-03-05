package lambdas
package deserialization

import tfdb._

object Typecheck{

  def read_t(t: Tree): Either[String, Typ] = t match {
    case Tree.TInt() =>
      Right(Typ(tint[TQ]))

    case Tree.TArr(t1, t2) => for {
      t1t <- read_t(t1)
      t2t <- read_t(t2)
    } yield Typ(t1t.typ -> t2t.typ)

    case _ =>
      Left(s"Not a type: $t")
  }

  implicit class EitherOp[A](o: Option[A]){
    def toEither[B](none: B): Either[B, A] =
      o.fold[Either[B, A]](Left(none))(Right(_))
  }

  def apply[P[_, _], Γ, E](tree: Tree, gamma: Γ)(implicit
      L: Lambda[P],
      G: Gamma[Γ, E]): Either[String, DynTerm[P, E]] = tree match {

    case Tree.Int(i) =>
      Right(DynTerm(tint[TQ], L.int(i)))

    case Tree.Add(e1, e2) => for {
      dt1 <- apply(e1, gamma).right
      dt2 <- apply(e2, gamma).right
      _dt1 <- dt1.typ[AsInt].cast[P[E, ?]](dt1.term).toEither(
        s"First operand of add, not an integer: ${dt1.typ}").right
      _dt2 <- dt2.typ[AsInt].cast[P[E, ?]](dt2.term).toEither(
        s"Second operand of add, not an integer: ${dt2.typ}").right
    } yield DynTerm(tint[TQ], L.add(_dt1, _dt2))

    case Tree.Var(name) =>
      G.findVar(name, gamma)

    case Tree.Lam(name, typ, body) => for {
      ty1 <- read_t(typ)
      db <- apply(body, (Gamma.Var(name, ty1.typ), gamma))
    } yield DynTerm(ty1.typ -> db.typ, L.lam(db.term))

    case Tree.App(ft, at) =>
      apply(ft, gamma).right.flatMap{ df => {
      val asArrow = df.typ[AsArrow]
      asArrow.eq.toEither(s"Not a lambda: ${df.typ}").right.flatMap{ case (tqT1, tqT2, eq) =>
      asArrow.cast[P[E, ?]](df.term).toEither(s"Should not happen").right.flatMap{ _df =>
      apply(at, gamma).right.flatMap{ da => {
      da.typ[As].cast[asArrow.T1, P[E, ?]](tqT1, da.term).toEither(s"Not argument: ${da.typ}").right.map{ _da =>
      DynTerm(tqT2, L.app(_df)(_da))
      }}}}}}}

    case _ =>
      Left(s"Typecheck error: $tree")
  }
}
